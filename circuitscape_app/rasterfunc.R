library(raster)

#' Create a raster for the ground, which is 'NA everywhere except roost coordinates'
#'  'used for resampling'.
#'
#' @param x
#' @param y
#' @param radius
#' @param resolution not image resolution, but relative proportion
#' @return raster for ground with roosts
create_ground_rast <- function(x, y, radius, resolution) {

    # First ground raster has min and max -inf and inf
    infgroundrast <- raster(
        xmn = x - radius, # set minimum x coordinate
        xmx = x + radius, # set maximum x coordinate
        ymn = y - radius, # set minimum y coordinate
        ymx = y + radius, # set maximum y coordinate
        res = c(resolution, resolution),
        crs = NA
    )
    roosts <- matrix(c(x, y), nrow = 1, ncol = 2)

    # Groundrast now has NA everywhere except roost x, y
    groundrast <- raster::rasterize(roosts, groundrast)
    return(groundrast)
}

#' Generate new rasters from several files
#'
#' @param surf
#' @param output_dir
#' @return distance_rasters a matrix of distance rasters
prep_lidar_tifs <- function(surf, output_dir, task_progress) {
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
    manhedge <- raster::buffer(manhedge, width=10, filename=paste(output_dir, "manhedge.asc", sep="/"), overwrite=TRUE)
    task_progress$incrementProgress(33)

    unmanhedge <- surf
    unmanhedge[surf<6 & surf>3] <- 1
    unmanhedge[surf>=6] <- NA
    unmanhedge[surf<=3] <- NA
    unmanhedge <- raster::buffer(unmanhedge, width=10, filename=paste(output_dir, "umanhedge.asc", sep="/"), overwrite=TRUE)
    task_progress$incrementProgress(33)

    tree <- surf
    tree[surf>=6] <- 1
    tree[surf<6] <- NA
    tree <- raster::buffer(tree,width=10, filename=paste(output_dir, "tree.asc", sep="/"), overwrite=TRUE)
    task_progress$incrementProgress(33)

    distance_rasters <- matrix(c(raster::distance(unmanhedge), 1, raster::distance(tree), 2, raster::distance(manhedge), 4), nrow=3, ncol=2)
    return(distance_rasters)
}

filter_binary_layer <- function(value) {
    fn <- function(x, y) {
        ifelse(y==1, value, x)
    }
    return(fn)
}

#' Calculate a resistance map for roads
#'
#' @param roads
#' @param groundrast
#' @param task_progress
#' @param algorithm_params
#' @return raster::raster object for roads
cal_road_resistance <- function(roads, groundrast, task_progress, algorithm_params) {

    buffer <- algorithm_params$roadResistance$buffer    # meters to road
    resmax <- c(algorithm_params$roadResistance$resmax) # max resistance of layers
    xmax <- c(algorithm_params$roadResistance$xmax)     # x is the slope value

    # buffer <- 200   # meters to road
    # Resmax <- c(10) # max resistance of layers
    # Xmax <- c(5)    # x is the slope value

    rbuff <- 0
    r <- raster::rasterize(roads, groundrast)
    task_progress$incrementProgress(33)
    road_distance <- raster::distance(r)
    task_progress$incrementProgress(33)
    resistance <- round(calc(road_distance,
                                function(d) {
                                    ifelse(d > buffer, rbuff, (((1 - (d/buffer))*0.5 + 0.5)^xmax)*resmax)
                                }
                            )+1,
                        digits=3)
    task_progress$incrementProgress(33)
    return(resistance)
}

#' Calculate a resistance map for rivers
#'
#' @param river
#' @param groundrast
#' @param task_progress
#' @param algorithm_params
#' @return raster::raster object for rivers
cal_river_resistance <- function(river, groundrast, task_progress, algorithm_params) {

    buffer <- algorithm_params$riverResistance$buffer    # meters to river
    resmax <- c(algorithm_params$riverResistance$resmax) # max resistance of layers
    xmax <- c(algorithm_params$riverResistance$xmax)     # x is the slope value

    resmax <- 1
    rbuff <- resmax
    r <- raster::rasterize(river, groundrast)
    task_progress$incrementProgress(33)
    river_distance <- raster::distance(r)
    task_progress$incrementProgress(33)
    river_distance[is.na(river_distance)] <- 0
    resistance <- round(calc(river_distance,
                                function(d) {
                                    ifelse(d > buffer, rbuff, ((d/buffer)^xmax)*resmax)
                                }
                            )+1,
                        digits=3) 
    resistance[is.na(resistance)] <- 1
    task_progress$incrementProgress(33)
    return(resistance)
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

ranked_resistance <- function(conductance, Rankmax, Resmax, Xmax) {
    resistance <- raster::calc(conductance, fun=function(rank) {ifelse(rank == Rankmax, Resmax, (rank/Rankmax)^Xmax * Resmax)})
    resistance <- resistance + 1
    resistance <- round(resistance, digits = 3)
    resistance[is.na(resistance) == TRUE] <- 1
    return(resistance)
}

landscapeResistance_lcm <- function(lcm, buildings, surfs, soft_surf, algorithm_params) {
    lidar_ranking <- c(-Inf, 0.5, 4,  # grass
                        0.5, 2.5, 3,  # scrub
                        2.5, Inf, 3)  # trees

    surfs$soft_surf[is.na(surfs$soft_surf)] <- 0
    conductance <- raster::reclassify(surfs$soft_surf, lidar_ranking) + lcm
    Ranking <- raster::maxValue(conductance)+1 #Max ranking: makes buildings the highest resistance
    rast <- buildings
    rast[!is.na(rast==TRUE)] <- 1.0 ## features
    rast[is.na(rast==TRUE)] <- 0.0  ## no features
    conductance <- raster::overlay(conductance, rast, fun=filter_binary_layer(Ranking))
}

distance_resistance <- function(buffer, Rankmax, Rbuff, Resmax, Xmax, distance_rasters, effect) { 
    # Calculates resistance raster given a distance raster
    raster_resistance <- distance_rasters[[1,1]]
    values(raster_resistance) <- 0
    print(paste("nrow(distance_rasters):", nrow(distance_rasters)))
    for (i in nrow(distance_rasters)) {
        print(paste("distance_rasters[i]:", i))
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

#' Calculate a linear resistance map
#'
#' @param surf
#' @param output_dir
#' @param taskProgess
#' @param algorithm_params
#' @return resistance: raster::raster object
linearResistance <- function(surf, output_dir, taskProgess, algorithm_params) {
    # TODO: what is linearResistance?
    distance_rasters <- prep_lidar_tifs(surf, output_dir, taskProgress)

    buffer <- algorithm_params$linearResistance$buffer
    Rankmax <- c(algorithm_params$linearResistance$rankmax)
    Resmax <- c(algorithm_params$linearResistance$resmax)
    Xmax <- c(algorithm_params$linearResistance$xmax)

    Rbuff <- Resmax[1]
    resistance <- distance_resistance(buffer, Rankmax, Rbuff, Resmax, Xmax, distance_rasters, "positive")
    resistance
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
#   writeRaster(point_irradiance,filename=paste("images/point_irradiance.tif"),overwrite=TRUE)
  return(point_irradiance)
}

lightdist <- function(xdist,ydist,zdist,theta=NULL) {
    xyzdist <- sqrt(xdist^2+ydist^2+zdist^2)
    return(1/(xyzdist^2))
}

lampResistance <- function(lamps, soft_surf, hard_surf, dtm, taskProgress, algorithmParameters, workingDir) {
    ext <- algorithmParameters$lampResistance$ext
    Resmax <- c(algorithmParameters$lampResistance$resmax)
    Xmax <- c(algorithmParameters$lampResistance$xmax)

    # ext <- 100
    # Resmax <- c(1e8)
    # Xmax <- c(1)

    point_irradiance <- calc_point_irradiance(lamps, soft_surf, hard_surf, dtm)
    taskProgress$incrementProgress(50)
    outputfile <- paste0(workingDir, "/light_resistance.asc")
    resistance <- light_resistance(Resmax, Xmax, point_irradiance, outputfile, extent_file)
    taskProgress$incrementProgress(50)
    return(resistance)
}

light_resistance <- function(Resmax, Xmax, rast, outputfile, extent_file) {
    print(Resmax)
    rast[is.na(rast==TRUE)] <- 0
    MaxPI <- maxValue(rast)
    raster_resistance <- round(calc(rast, fun=function(PI) {((PI/MaxPI)^Xmax)*Resmax}) + 1, digits = 3)
    raster_resistance[is.na(raster_resistance) == TRUE] <- 1
    # writeRaster(raster_resistance, filename=outputfile, NAflag=-9999, overwrite=TRUE)
    raster_resistance
}

createCircles <- function(groundrast, algorithmParameters) {
    roost <- c(algorithmParameters$roost$x, algorithmParameters$roost$y)
    radius <- algorithmParameters$roost$radius

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
