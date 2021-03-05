#!/usr/bin/env Rscript

library(sf)
library(raster)
library(rpostgis)
library(JuliaCall)

library(glue)

options(shiny.port=8100)

create_ext <- function(roost, radius) {
    xmin = roost[1]-radius
    xmax = roost[1]+radius
    ymin = roost[2]-radius
    ymax = roost[2]+radius
    extent(xmin, xmax, ymin, ymax)
}

ground_rast <- function(roost, radius, resolution) {
  # Generates a ground raster: NA everywhere except roost coordinates, used for resampling
  groundrast <- raster(
        xmn = roost[1] - radius, # set minimum x coordinate
        xmx = roost[1] + radius, # set maximum x coordinate
        ymn = roost[2] - radius, # set minimum y coordinate
        ymx = roost[2] + radius, # set maximum y coordinate
        res = c(resolution,resolution),
        crs = NA
    ) 
  roosts <- matrix(c(roost[1],roost[2]), nrow = 1, ncol = 2)
  groundrast <- rasterize(roosts,groundrast)
  return(groundrast)
}

build_query_string <- function(tableName, ext) {
    xmin = attr(ext, "xmin")
    xmax = attr(ext, "xmax")
    ymin = attr(ext, "ymin")
    ymax = attr(ext, "ymax")
    glue("
        SELECT ST_Multi( ST_Intersection({tableName}.geom, ST_MakeEnvelope({xmin}, {ymin}, {xmax}, {ymax}, 27700)) ) AS geom
        FROM {tableName}
        WHERE {tableName}.geom && ST_MakeEnvelope({xmin}, {ymin}, {xmax}, {ymax}, 27700);
    ")
}

read_db_vector <- function(tableName, ext) {
    driver <- dbDriver("PostgreSQL")
    connection <- dbConnect(driver, dbname="os", port=5433)
    query = build_query_string(tableName, ext)
    results_sf <- pgGetGeom(connection, query=query)
    dbDisconnect(connection)
    return(results_sf)
}

roadResistance <- function(roads, groundrast) { 
    buffer <- 200   # meters to road
    Resmax <- c(10) # max resistance of layers
    Xmax <- c(5)    # x is the slope value
    Rbuff <- 0
    r <- rasterize(roads, groundrast)
    roadDistance <- distance(r)
    resistance <- round(calc(roadDistance, function(Distance) {ifelse(Distance > buffer, Rbuff, (((1 - (Distance/buffer))*0.5 + 0.5)^Xmax)*Resmax)})+1, digits=3)
    return(resistance)
}

riverResistance <- function(river, groundrast) {  
    buffer <- 10
    Rankmax <- 1
    Resmax <- c(2000)
    Xmax <- c(4)
    Rbuff = Resmax
    r <- rasterize(river, groundrast)
    riverDistance <- distance(r)
    riverDistance[is.na(riverDistance)] <- 0
    resistance <- round(calc(riverDistance, function(Distance) {ifelse(Distance > buffer, Rbuff, ((Distance/buffer)^Xmax)*Resmax)})+1, digits=3) # this is the function: distance/ranks in traffic 
    resistance[is.na(resistance)] <- 1
    return(resistance)
}

create_raster_query_boundary <- function(ext) {
    xmin = attr(ext, "xmin")
    xmax = attr(ext, "xmax")
    ymin = attr(ext, "ymin")
    ymax = attr(ext, "ymax")
    boundary = c(ymax, ymin, xmax, xmin)
    return(boundary)
}

read_db_raster <- function(table, ext) {
    name = c("public", table)
    boundary = create_raster_query_boundary(ext)
    driver <- dbDriver("PostgreSQL")
    connection <- dbConnect(driver, dbname="os", port=5433)
    raster <- pgGetRast(connection, name=name, boundary=boundary)
    dbDisconnect(connection)
    return(raster)
}

calc_surfs <- function(dtm, dsm, buildings) {
    # Returns surf, soft and hard surface rasters
    surf <- dsm - dtm

    soft_surf <- (buildings+1)*surf
    soft_surf[is.na(soft_surf)] <- 0
    soft_surf <- surf - soft_surf

    hard_surf <- buildings
    hard_surf[is.na(hard_surf)] <- 1
    hard_surf<- hard_surf * surf
    hard_surf <- abs(hard_surf - surf)

    return(c("surf"=surf, "soft_surf"=soft_surf, "hard_surf"=hard_surf))
}

filter_binary_layer <- function(value) {
    fn <- function(x, y) {
        ifelse(y==1, value, x)
    }
    return(fn)
}

ranked_resistance <- function(conductance, Rankmax, Resmax, Xmax) {
    resistance <- calc(conductance, fun=function(rank) {ifelse(rank == Rankmax, Resmax, (rank/Rankmax)^Xmax * Resmax)})
    resistance <- resistance + 1
    resistance <- round(resistance, digits = 3)
    resistance[is.na(resistance) == TRUE] <- 1
    return(resistance)
}

landscapeResistance_lcm <- function(lcm, buildings, surfs, soft_surf) {
    lidar_ranking <- c(-Inf, 0.5, 4,  # grass
                        0.5, 2.5, 3,  # scrub
                        2.5, Inf, 3)  # trees
    Resmax <- 100
    Xmax <- 5
    surfs$soft_surf[is.na(surfs$soft_surf)] <- 0
    conductance <- reclassify(surfs$soft_surf, lidar_ranking) + lcm
    Ranking <- maxValue(conductance)+1 #Max ranking: makes buildings the highest resistance
    rast <- buildings
    rast[!is.na(rast==TRUE)] <- 1.0 ## features
    rast[is.na(rast==TRUE)] <- 0.0  ## no features
    conductance <- overlay(conductance, rast, fun=filter_binary_layer(Ranking))
#     resistance <- ranked_resistance(conductance, Rankmax, Resmax, Xmax)
}

prep_lidar_tifs <- function(surf, output_dir) { 
    # Generates new rasters containing locations of:
    # - manhedge.asc: managed hedgerows (height = 1m-3m above ground)
    # - unmanhedge.asc: unmanaged hedgerows (height = 3m-6m above ground)
    # - tree.asc: trees (height = 6m+ above ground)
    # Add a buffer around all features of width 10m
    # Then calculate the distance in each cell to the nearest feature
    # Distance raster files are saved in directory output_dir

    manhedge <- surf
    manhedge[surf<3 & surf>1] <- 1
    manhedge[surf>=3] <- NA
    manhedge[surf<=1] <- NA
    manhedge <- buffer(manhedge, width=10, filename=paste(output_dir, "manhedge.asc", sep="/"), overwrite=TRUE)

    unmanhedge <- surf
    unmanhedge[surf<6 & surf>3] <- 1
    unmanhedge[surf>=6] <- NA
    unmanhedge[surf<=3] <- NA
    unmanhedge <- buffer(unmanhedge, width=10, filename=paste(output_dir, "umanhedge.asc", sep="/"), overwrite=TRUE)

    tree <- surf
    tree[surf>=6] <- 1
    tree[surf<6] <- NA
    tree <- buffer(tree,width=10, filename=paste(output_dir, "tree.asc",sep="/"), overwrite=TRUE)

    distance_rasters <- matrix(c(distance(unmanhedge), 1, distance(tree), 2, distance(manhedge), 4), nrow=3, ncol=2)
    return(distance_rasters)
}

distance_resistance <- function(buffer, Rankmax, Rbuff, Resmax, Xmax, distance_rasters, effect) { 
    # Calculates resistance raster given a distance raster
    raster_resistance <- distance_rasters[[1,1]]
    values(raster_resistance) <- 0
    for (i in nrow(distance_rasters)) {
        rast <- distance_rasters[[i,1]]
        Ranking <- distance_rasters[[i,2]]
        rast[is.na(rast) == TRUE] <- 0
        if (effect == "negative") {
            partial_resistance <- round(calc(rast, function(Distance) {ifelse(Distance > buffer, Rbuff, (((1 - (Distance/buffer))*0.5 + 0.5 *(Ranking/Rankmax))^Xmax)*Resmax)})+1, digits=3) # this is the function: distance/ranks in traffic 
        }
        else {
            partial_resistance <- round(calc(rast, function(Distance) {ifelse(Distance > buffer, Rbuff, ((((Distance/buffer))*0.5 + 0.5 *(Ranking/Rankmax))^Xmax)*Resmax)})+1, digits=3) # this is the function: distance/ranks in traffic 
        }
        raster_resistance <- overlay(partial_resistance, raster_resistance, fun = max)  
    }
    raster_resistance[is.na(raster_resistance) == TRUE] <- 1
    raster_resistance
}

linearResistance <- function(surf, output_dir) { 
    distance_rasters <- prep_lidar_tifs(surf, output_dir)
    buffer <- 10
    Rankmax <- 4
    Resmax <- c(22000)
    Xmax <- c(3)
    Rbuff <- Resmax[1]
    resistance <- distance_resistance(buffer, Rankmax, Rbuff, Resmax, Xmax, distance_rasters, "positive")
    resistance
}

lampResistance <- function(lamps, soft_surf, hard_surf, dtm) {
    ext <- 100
    Resmax <- c(1e8)
    Xmax <- c(1)
    point_irradiance <- calc_point_irradiance(lamps, soft_surf, hard_surf, dtm)
    outputfile <- "images/light_resistance.asc"
    resistance <- light_resistance(Resmax, Xmax, point_irradiance, outputfile, extent_file)
    return(resistance)
}

lightdist <- function(xdist,ydist,zdist,theta=NULL) {
    xyzdist <- sqrt(xdist^2+ydist^2+zdist^2)
    return(1/(xyzdist^2))
}

calc_point_irradiance <- function(lamps, soft_surf, hard_surf, terrain) {
  point_irradiance <- soft_surf
  values(point_irradiance) <- 0 
  ext <- 100
  sensor_ht <- 2.5
  absorbance <- 0.5  
  for(lamp in 1:dim(lamps)[1]) {
    
    x <- lamps$x[lamp]
    y <- lamps$y[lamp]
    z <- lamps$z[lamp]
    
    ### find dimensions of area of influence of light
    row <- rowFromY(hard_surf,min(max(y,ymin(hard_surf)),ymax(hard_surf)))
    col <- colFromX(hard_surf,max(min(x,xmax(hard_surf)),xmin(hard_surf)))
    col_min <- max(col-ext,1)
    col_max <- min(col+ext,colFromX(hard_surf,xmax(hard_surf)))
    col_lamp <- col-col_min
    row_min <- max(row-ext,1)
    row_max <- min(row+ext,rowFromY(hard_surf,ymin(hard_surf)))
    row_lamp <- row-row_min
    nrows <- row_max-row_min
    ncols <- col_max-col_min
    
    ### extract terrain data for area of influence of light 
    hard_block <- array(getValuesBlock(hard_surf,row=row_min,nrows=nrows,col=col_min,ncols=ncols),c(nrows,ncols))
    soft_block <- array(getValuesBlock(soft_surf,row=row_min,nrows=nrows,col=col_min,ncols=ncols),c(nrows,ncols))
    terrain_block <- array(getValuesBlock(terrain,row=row_min,nrows=nrows,col=col_min,ncols=ncols),c(nrows,ncols))
    hard_block[is.na(hard_block==TRUE)] <- 0
    soft_block[is.na(soft_block==TRUE)] <- 0
    terrain_block[is.na(terrain_block==TRUE)] <- 0
    point_irrad <- array(0,c(ncols,nrows))
    ### find values for irradiance on a horizontal plane and sphere
    if (ncols==200&nrows==200) {
      for(xx in 1:ncols) {
        for (yy in 1:nrows) {
          xdist <- col_lamp-xx
          ydist <- row_lamp-yy
          xydist <- sqrt(xdist^2+ydist^2)
          zdist <- (terrain_block[row_lamp,col_lamp]+z)-(terrain_block[yy,xx]+sensor_ht)
          xyzdist <- sqrt(xydist^2+zdist^2)
          dist <- floor(xydist+0.5)
          if (xydist<=ext && zdist>0 && is.na(hard_block[yy,xx])==FALSE && dist>0) {
            shadow <- 1
            shading <- 0
            for (d in 1:dist) {
              if(hard_block[as.integer(yy+(ydist)*(d/dist)),as.integer(xx+(xdist)*(d/dist))] >= (terrain_block[yy,xx]+sensor_ht+(d/dist)*zdist)) {
                shadow <- 0
                break
              }
              if(soft_block[as.integer(yy+(ydist)*(d/dist)),as.integer(xx+(xdist)*(d/dist))] >= (terrain_block[yy,xx]+sensor_ht+(d/dist)*zdist)) {
                shading <- shading + xyzdist/xydist
              }
              point_irrad[xx,yy] <- (1/(10^(absorbance*shading)))*shadow*lightdist(xdist,ydist,zdist)
            }
          }
        }
      }
      point_values <- raster(point_irrad,xmn=xFromCol(point_irradiance,col_min),ymn=yFromRow(point_irradiance,row_max),xmx=xFromCol(point_irradiance,col_max),ymx=yFromRow(point_irradiance,row_min))
      origin(point_values) <- origin(point_irradiance)
      point_irradiance <- mosaic(point_irradiance,point_values,fun=sum)
      removeTmpFiles()
    }
  }
  writeRaster(point_irradiance,filename=paste("images/point_irradiance.tif"),overwrite=TRUE)
  return(point_irradiance)
}

light_resistance <- function(Resmax, Xmax, rast, outputfile, extent_file) {
    print(Resmax)
    rast[is.na(rast==TRUE)] <- 0
    MaxPI <- maxValue(rast)
    raster_resistance <- round(calc(rast, fun=function(PI) {((PI/MaxPI)^Xmax)*Resmax}) + 1, digits = 3)
    raster_resistance[is.na(raster_resistance) == TRUE] <- 1
    writeRaster(raster_resistance, filename=outputfile, NAflag=-9999, overwrite=TRUE)
    raster_resistance
}

load_lamps <- function(lightsFilename, roost, radius) {
    lamps <- read.csv(file=lightsFilename, col.names=c("x", "y", "z"))
    colnames(lamps) <- c("x", "y", "z")
    ext <- 100
    lamps <- lamps[(lamps$x-roost[1])^2 + (lamps$y-roost[2])^2 < (radius+ext)^2,]
}

createCircles <- function(groundrast, roost, radius) {
    circles = groundrast
    values(circles) = 0
    for (r in seq(50,radius,50)) {
        angle = 2*pi*(0:(3*r))/(3*r)
        df <- data.frame( x = roost[1]+r*sin(angle), y = roost[2]+r*cos(angle))
        points = SpatialPoints(df, proj4string=CRS(as.character(NA)), bbox = NULL) 
        circles = circles + rasterize(points, groundrast, background=0)
    }
    circles[circles>0]=1
    circles
}

generate <- function(roost, radius, lightsFilename, verbose=FALSE, saveImages=FALSE) {
    if (verbose) {
        print("GENERATE")
        print(glue("roost=({roost[1]}, {roost[2]}); radius={radius}m; lightsFilename={lightsFilename}"))
    }

    resolution = 1
    ext <- create_ext(roost, radius)

    # Ground Raster
    groundrast <- ground_rast(roost, radius, resolution)
    writeRaster(groundrast, "circuitscape/ground.asc", overwrite=TRUE)

    # Ordnance Survey Vector Data
    if (verbose) { print("Querying OS vector data...") }
    roads <- read_db_vector("roads", ext)
    rivers <- read_db_vector("rivers", ext)
    buildings <- read_db_vector("buildings", ext)
    if (saveImages) { png("images/roads.png"); plot(roads, axes=TRUE) }
    if (saveImages) { png("images/rivers.png"); plot(rivers, axes=TRUE) }
    if (saveImages) { png("images/buildings.png"); plot(buildings, axes=TRUE) }

    # Rasterize Buildings
    if (verbose) { print("Rasterizing buildings...") }
    buildings <- rasterize(buildings, groundrast)
    buildings[!is.na(buildings)] <- 1
    if (saveImages) { png("images/rasterizedBuildings.png"); plot(buildings, axes=TRUE) }

    # Road Resistance
    if (verbose) { print("Calculating road resistance...") }
    roadRes <- roadResistance(roads, groundrast)
    if (saveImages) { png("images/roadRes.png"); plot(roadRes, axes=TRUE) }

    # River Resistance
    if (verbose) { print("Calculating river resistance...") }
    riverRes = riverResistance(rivers, groundrast)
    if (saveImages) { png("images/riverRes.png"); plot(riverRes, axes=TRUE) }

    # LIDAR
    if (verbose) { print("Querying LIDAR rasters...") }
    dtm = read_db_raster("dtm", ext)
    dsm = read_db_raster("dsm", ext)
    if (saveImages) { png("images/dtm.png"); plot(dtm, axes=TRUE) }
    if (saveImages) { png("images/dsm.png"); plot(dsm, axes=TRUE) }

    if (verbose) { print("Resampling LIDAR rasters...") }
    r_dtm = resample(dtm, groundrast)
    r_dsm = resample(dsm, groundrast)

    if (verbose) { print("Calculating surfaces...") }
    surfs <- calc_surfs(r_dtm, r_dsm, buildings)

    if (verbose) { print("Querying and resampling LCM raster...") }
    lcm = read_db_raster("lcm", ext)
    lcm_r <- resample(lcm, groundrast)
    if (saveImages) { png("images/lcm.png"); plot(lcm, axes=TRUE) }
    if (saveImages) { png("images/lcm_r.png"); plot(lcm_r, axes=TRUE) }

    if (verbose) { print("Calculating landscape resistance...") }
    landscapeRes = landscapeResistance_lcm(lcm_r, buildings, surfs, surfs$soft_surf)
    if (saveImages) { png("images/landscapeRes.png"); plot(landscapeRes, axes=TRUE) }

    if (verbose) { print("Calculating linear resistance...") }
    linearRes = linearResistance(surfs$soft_surf, "./images")
    if (saveImages) { png("images/linearRes.png"); plot(linearRes, axes=TRUE) }

    if (verbose) { print("Loading lamps...") }
    lamps <- load_lamps(lightsFilename, roost, radius)
    if (saveImages) { png("images/lamps.png"); plot(lamps$x,lamps$y, axes=TRUE) }

    if (verbose) { print("Calculating lamp resistance...") }
    lampRes <- lampResistance(lamps, surfs$soft_surf, surfs$hard_surf, dtm)
    if (saveImages) { png("images/lampRes.png"); plot(lampRes, axes=TRUE) }

    if (verbose) { print("Calculating total resistance...") }
    totalRes = lampRes + roadRes + linearRes + riverRes + landscapeRes
    writeRaster(totalRes, "circuitscape/resistance.asc", overwrite=TRUE)
    if (saveImages) { png("images/totalRes.png"); plot(totalRes, axes=TRUE) }

    if (verbose) { print("Generating circles raster...") }
    circles = createCircles(groundrast, roost, radius)
    writeRaster(circles, "circuitscape/source.asc", NAflag=-9999, overwrite=TRUE)
    if (saveImages) { png("images/circles.png"); plot(circles, axes=TRUE) }

    if (verbose) { print("Calculating Circuitscape...") }
    julia_install_package_if_needed("Circuitscape") # if you don't already have the package installed
    julia_library("Circuitscape")                   # make sure Circuitscape is available
    julia_call("compute", "cs.ini", need_return="None")

    if (verbose) { print("Generating current raster...") }
    current = raster("cs_out_curmap.asc")
    logCurrent = log(current)
    if (verbose) { print("...done") }
    if (saveImages) { png("images/current.png"); plot(current, axes=TRUE) }
    if (saveImages) { png("images/logCurrent.png"); plot(logCurrent, axes=TRUE) }
}

generate(
    roost=c(274257,66207),
    radius=300,
    lightsFilename="gis-layers/lights.csv",
    verbose=TRUE,
    saveImages=FALSE
)
