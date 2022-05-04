library(raster)
library(logger)

rasterize_buildings <- function(buildings, groundrast) {

    logger::log_info("Rasterizing buildings")
    print(buildings)
    if (length(buildings) > 0) {
        buildings_raster <- raster::rasterize(buildings, groundrast)
    } else {
        logger::log_info("No buildings")
        buildings_raster <- raster::raster()
        values(buildings_raster) <- NA
    }
    # TODO: changed this to 0, should it be 1?
    # buildings_raster[!is.na(buildings_raster)] <- 1
    buildings_raster[!is.na(buildings_raster)] <- 0
    buildings_raster

}

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
    infgroundrast <- raster::raster(
        xmn = x - radius, # set minimum x coordinate
        xmx = x + radius, # set maximum x coordinate
        ymn = y - radius, # set minimum y coordinate
        ymx = y + radius, # set maximum y coordinate
        res = c(resolution, resolution),
        crs = NA
    )
    roosts <- matrix(c(x, y), nrow = 1, ncol = 2)
    # Groundrast now has NA everywhere except roost x, y
    groundrast <- raster::rasterize(roosts, infgroundrast)
    return(groundrast)
}

#' Get a function that takes x, y, and returns value if y is 1, otherwise x
#'  Can be used with a bitmask; if f(X, B) gives X, with 1s replaced with value
#'
#' @param value value to set 1s to
#' @return function f
filter_binary_layer <- function(value) {
    fn <- function(x, y) {
        ifelse(y==1, value, x)
    }
    return(fn)
}

#' Rasterize some data, and for any NAs, take the distance to the nearest non-NA
#' 
#' @param data spatial dataframe
#' @param groundrast base raster to use
#' @return distance a distance raster 
cal_distance_raster <- function(data, groundrast) {
    r <- raster::rasterize(data, groundrast)
    v <- values(r)
    if (length(v[is.na(v)]) == 0) {
        print(v)
        logger::log_error("Cannot calculate a distance raster on data without any NA values. This probably should not have happened. Do the vector features cover every pixel?")
        stop("Cannot call raster::distance on data without NAs")
    }
    distance <- raster::distance(r)
    return(distance)
}

#' Calculate a resistance map for roads
#'
#' @param roads
#' @param groundrast
#' @param buffer meters to river
#' @param resmax maximum resistance of "layers"
#' @param xmax "slope value"
#' @return raster::raster object for roads
cal_road_resistance <- function(roads, groundrast, buffer, resmax, xmax) {

    # If empty, resistance is min, which seems to be 1
    if (length(roads)  == 0) {
        resistance <- groundrast
        values(resistance) <- 1
        return(resistance)
    }

    road_distance <- cal_distance_raster(roads, groundrast)

    resistance <- round(calc(road_distance,
                                function(d) {
                                    ifelse(d > buffer, 0, (((1 - (d/buffer))*0.5 + 0.5)^xmax)*resmax)
                                }
                            )+1,
                        digits=3)

    return(resistance)
}

# TODO: what is rbuff
#' Calculate a resistance map for rivers
#'
#' @param river sp-class (SpatialPoints*, SpatialMultiPoints*, SpatialLines*, or SpatialPolygons*)
#' @param groundrast
#' @param buffer meters to river
#' @param resmax maximum resistance of "layers"
#' @param xmax "slope value"
#' @param rbuff ???
#' @return raster::raster object for rivers
cal_river_resistance <- function(river, groundrast, buffer, resmax, xmax, rbuff=1) {

    # If empty geom, resistance seems to be rbff + 1
    if (length(river)  == 0) {
        resistance <- groundrast
        values(resistance) <- rbuff + 1
        return(resistance)
    }

    resmax <- 1

    river_distance <- cal_distance_raster(river, groundrast)

    # TODO: why get nans
    river_distance[is.na(river_distance)] <- 0

    # TODO: what is
    resistance <- round(calc(river_distance,
                                function(d) {
                                    ifelse(d > buffer, rbuff, ((d/buffer)^xmax)*resmax)
                                }
                            )+1,
                        digits=3)

    # TODO: why?
    resistance[is.na(resistance)] <- 1

    return(resistance)
}

#' Create a disk at x, y of a given radius, to be used as a mask
#' 
#' @param groundrast base raster to extract circles from
#' @param x
#' @param y
#' @param radius
#' @return circles raster
create_disk_mask <- function(groundrast, x, y, radius) {

    disk <- groundrast
    raster::values(disk) <- NA

    x <- 0.5 * (disk@extent@xmin + disk@extent@xmax)
    y <- 0.5 * (disk@extent@ymin + disk@extent@ymax)

    disk <- distanceFromPoints(disk, c(x,y))
    disk2 <- disk
    values(disk2) <- (values(disk) < radius)
    values(disk2)[values(disk2) == FALSE] <- NA

    disk2
}

#' Create raster with concentric circles
#' 
#' @param groundrast base raster to extract circles from
#' @param x
#' @param y
#' @param radius
#' @return circles raster
create_circles <- function(groundrast, x, y, radius) {

    circles <- groundrast
    raster::values(circles) <- 0
    # TODO: add in an exception if radius is too small
    # TODO: why 50?
    for (r in seq(50, radius, 50)) {
        angle <- 2 * pi * (0:(3 * r )) / (3*r)
        df <- data.frame(x=x+r*sin(angle), y=y+r*cos(angle))
        # TODO: change from spatialpoints to spatiallines
        points <- sp::SpatialPoints(df, proj4string=CRS(as.character(NA)), bbox = NULL)
        circles <- circles + raster::rasterize(points, groundrast, background=0)
    }
    circles[circles>0] <- 1
    circles
}

# TODO: checkout where these formulae came from
#' Calculate a resistance map given a distance raster
#'
#' @param buffer distance from features
#' @param rankmax
#' @param rbuff
#' @param resmax
#' @param xmx
#' @param distance_rasters
#' @param effect can be "negative" or something else
#' @return resistance raster
distance2resistance <- function(buffer, rankmax, rbuff, resmax, xmax, distance_rasters, effect) {
    raster_resistance <- distance_rasters[[1, 1]]
    raster::values(raster_resistance) <- 0
    for (i in nrow(distance_rasters)) {
        rast <- distance_rasters[[i, 1]]
        Ranking <- distance_rasters[[i, 2]]
        rast[is.na(rast) == TRUE] <- 0
        # TODO: change this
        if (effect == "negative") {
            partial_resistance <- round(raster::calc(rast, function(Distance) {ifelse(Distance > buffer, rbuff, (((1 - (Distance/buffer))*0.5 + 0.5 *(Ranking/rankmax))^xmax)*resmax)})+1, digits=3) # this is the function: distance/ranks in traffic 
        }
        else {
            partial_resistance <- round(raster::calc(rast, function(Distance) {ifelse(Distance > buffer, rbuff, ((((Distance/buffer))*0.5 + 0.5 *(Ranking/rankmax))^xmax)*resmax)})+1, digits=3) # this is the function: distance/ranks in traffic 
        }
        raster_resistance <- raster::overlay(partial_resistance, raster_resistance, fun = max)  
    }
    raster_resistance[is.na(raster_resistance) == TRUE] <- 1
    raster_resistance
}

# TODO: will crash if the surf does not have all these values in -- presumably will crash therefore if not got hedges, etc.
# TODO: so needs to handle that
# TODO: rename
# TODO: several bugs present in this -- needs thorough tests
#' Generate new distance rasters
#'
#' @param surf a raster representing the surface
#' @return distance_rasters a matrix of distance rasters
prep_lidar_rasters <- function(surf) {
    # TODO: seems to assume that surface map is either managed hedge, umanaged, or tree? How do we know only those 3?
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
    # TODO: change to a warning
    hedge_check <- (NA %in% manhedge@data@values) && (1 %in% manhedge@data@values)
    stopifnot("Surface either contains no manhedges or is all manhedges" = hedge_check)
    #manhedge <- raster::buffer(manhedge, width=10, filename=paste(output_dir, "manhedge.asc", sep="/"), overwrite=TRUE)
    manhedge <- raster::buffer(manhedge, width=10)

    unmanhedge <- surf
    unmanhedge[surf<6 & surf>3] <- 1
    unmanhedge[surf>=6] <- NA
    unmanhedge[surf<=3] <- NA
    hedge_check <- (NA %in% unmanhedge@data@values) && (1 %in% unmanhedge@data@values)
    stopifnot("Surface either contains no unmanhedges or is all unmanhedges" = hedge_check)
    unmanhedge <- raster::buffer(unmanhedge, width=10)

    tree <- surf
    tree[surf>=6] <- 1
    tree[surf<6] <- NA
    tree <- raster::buffer(tree, width=10)

    hedge_check <- (NA %in% tree@data@values) && (1 %in% tree@data@values)
    stopifnot("Surface either contains no trees or is all trees" = hedge_check)
    # TODO: I suspect this may be wrong -- should be raster raster raster 1 2 4 -- change back if so?
    # TODO: further, why do they have a different order to the order of calculation?
    # distance_rasters <- matrix(c(raster::distance(unmanhedge), 1, raster::distance(tree), 2, raster::distance(manhedge), 4), nrow=3, ncol=2)
    distance_rasters <- matrix(c(raster::distance(unmanhedge), raster::distance(tree), raster::distance(manhedge), 1, 2, 4), nrow=3, ncol=2)
    return(distance_rasters)
}

# TODO: what are soft and hard surfaces?
#   Soft surf appears to have trees and hedges extracted later
#' Given a dtm, dsm raster and buildings vector, calculate surfaces, and soft and hard surfaces
calc_surfs <- function(dtm, dsm, buildings) {
    # Returns surf, soft and hard surface rasters
    surf <- dsm - dtm

    soft_surf <- (buildings+1)*surf
    soft_surf[is.na(soft_surf)] <- 0
    soft_surf <- surf - soft_surf

    hard_surf <- buildings
    hard_surf[is.na(hard_surf)] <- 1
    hard_surf <- hard_surf * surf
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

# TODO: take just soft surf not all
# TODO: what is lcm? soft surface?
#' Generate landscape resistance "lcm"
#' 
#' @param lcm ???
#' @param buildings buildings raster
#' @param surfs vector of surface rasters
#' @returns conductance raster
get_landscape_resistance_lcm <- function(lcm, buildings, surfs) {
    # TODO: check this representation
    lidar_ranking <- c(-Inf, 0.5, 4,  # grass
                        0.5, 2.5, 3,  # scrub
                        2.5, Inf, 3)  # trees

    surfs$soft_surf[is.na(surfs$soft_surf)] <- 0
    conductance <- raster::reclassify(surfs$soft_surf, lidar_ranking) + lcm
    Ranking <- raster::maxValue(conductance) + 1 #Max ranking: makes buildings the highest resistance
    rast <- buildings
    rast[!is.na(rast==TRUE)] <- 1.0 ## features
    rast[is.na(rast==TRUE)] <- 0.0  ## no features
    conductance <- raster::overlay(conductance, rast, fun=filter_binary_layer(Ranking))
}

#' Calculate a linear resistance map
#'
#' @param surf
#' @param buffer
#' @param rankmax
#' @param resmax
#' @param xmax
#' @return resistance: raster::raster object
get_linear_resistance <- function(surf, buffer, rankmax, resmax, xmax) {
    # TODO: what is linearResistance?
    distance_rasters <- prep_lidar_rasters(surf)
    rbuff <- resmax[1]
    resistance <- distance2resistance(buffer, rankmax, rbuff, resmax, xmax, distance_rasters, "positive")
    resistance
}

#' Given x, [lb, ub], return x if in [lb, ub], or lb, ub if <lb, )>ub
bound_val <- function(x, lb, ub) {
    v <- min(max(x, lb), ub)
    return(v)
}

# TODO: need some thorough test cases to make sure this is working as expected, see TODOs
# TODO: extent should not be hard coded
# TODO: check ri_lamp and cj_lamp, does not look correct
# TODO: more tests for this one
#' Calculate the area lit, given coordinates of a light, and a hard surface map
#' @param x
#' @param y
#' @param z
#' @param hard_surf
#' @param delta integer delta giving number of indices to include around light
#' @return a list of coordinates relative to the hard surface, which is a sort of mask
#'      ri_min (max), the min (max) row index on the surface that is lit
#'      cj_min (max), the min (max) col index on the surface that is lit
#'      ri_lamp, cj_lamp, the row index and col index for the lamp
#'      nrows, ncols, the n rows and cols for the lit area
cal_light_surface_indices <- function(x, y, z, hard_surf, delta) {

    bounded_y <- bound_val(y, raster::ymin(hard_surf), raster::ymax(hard_surf))
    bounded_x <- bound_val(x, raster::xmin(hard_surf), raster::xmax(hard_surf))
    ri <- raster::rowFromY(hard_surf, bounded_y)
    cj <- raster::colFromX(hard_surf, bounded_x)

    # Get the minimum index which is lit (within delta)
    cj_min <- max(cj - delta, 1)
    cj_max <- min(cj + delta, hard_surf@ncols)
    cj_lamp <- cj-cj_min

    ri_min <- max(ri - delta, 1)
    ri_max <- min(ri + delta, hard_surf@nrows)
    ri_lamp <- ri-ri_min

    nrows <- ri_max-ri_min
    ncols <- cj_max-cj_min

    return(list(ri_min=ri_min, ri_max=ri_max, cj_min=cj_min, cj_max=cj_max, ri_lamp=ri_lamp, cj_lamp=cj_lamp, nrows=nrows, ncols=ncols))
}

#' Get data from the lit terrain area
#' 
#' @param hard_surf
#' @param soft_surf
#' @param terrain
#' @param ri_min
#' @param rj_min
#' @param ncols
#' @param nrows
#' @return list of terrain blocks
get_blocks <- function(hard_surf, soft_surf, terrain, ri_min, cj_min, ncols, nrows) {
    logger::log_info("Fetching a block.")
    hard_block <- array(raster::getValuesBlock(hard_surf, row=ri_min, nrows=nrows, col=cj_min, ncols=ncols), c(nrows, ncols))
    soft_block <- array(raster::getValuesBlock(soft_surf, row=ri_min, nrows=nrows, col=cj_min, ncols=ncols), c(nrows, ncols))
    terrain_block <- array(raster::getValuesBlock(terrain, row=ri_min, nrows=nrows, col=cj_min, ncols=ncols), c(nrows, ncols))
    hard_block[is.na(hard_block==TRUE)] <- 0
    soft_block[is.na(soft_block==TRUE)] <- 0
    terrain_block[is.na(terrain_block==TRUE)] <- 0
    return(list(soft_block=soft_block, hard_block=hard_block, terrain_block=terrain_block))
}

# TODO: parallelize?
# TODO: break down into further subfunctions
#' Calculate irradiance for a single position, fill arr in place
#' 
#' @param arr array to calculate on
#' @param ri_lamp row index for lamp
#' @param cj_lamp col index for lamp
#' @param z height of lamp
#' @param ncols
#' @param nrows
#' @param min_xy_d light distance threshold
#' @param terrain_block
#' @param hard_block
#' @param soft_block
cal_irradiance_arr <- function(ri_lamp, cj_lamp, z, ncols, nrows, min_xy_d, terrain_block, hard_block, soft_block) {
    # TODO: make params
    sensor_ht <- 2.5
    absorbance <- 0.5
    # Populate a new row with size ncols, nrows
    # ? TODO: should be array(1, c(1,2)) for 2 cols, reverse
    # arr <- array(0, c(ncols, nrows))
    arr <- array(0, c(nrows, ncols))
    for (cj in 1:ncols) {
        for (ri in 1:nrows) {
            # ? TODO: should be abs here, xdist can be negative, ok for square, but later is not squared
            xdist <- cj_lamp - cj
            ydist <- ri_lamp - ri
            xydist <- sqrt(xdist^2 + ydist^2)
            zdist <- (terrain_block[ri_lamp, cj_lamp] + z) - (terrain_block[ri, cj] + sensor_ht)
            xyzdist <- sqrt(xydist^2 + zdist^2)
            # why bother with this if cj and ci exceeds distance? why not just do a loop that uses distance
            dist <- floor(xydist + 0.5)
            if (xydist <= min_xy_d && zdist > 0 && is.na(hard_block[ri,cj]) == FALSE && dist > 0) {
                shadow <- 1
                shading <- 0
                # print(paste(ri, cj, ydist, xdist, dist, zdist, xydist, xyzdist))
                for (d in 1:dist) {
                    # TODO: round to the nearest pixel instead, it's clearer
                    dii <- as.integer(ri + (ydist * d / dist))
                    djj <- as.integer(cj + (xdist * d / dist))
                    # height of the shadow at this point
                    hiijj <- terrain_block[ri, cj] + sensor_ht + (d/dist) * zdist
                    # why mention sensor height?
                    if (hard_block[dii, djj] >= hiijj) {
                        shadow <- 0
                        break
                    }
                    if (soft_block[dii, djj] >= hiijj) {
                        shading <- shading + xyzdist/xydist
                    }
                    arr[ri,cj] <- (1/(10^(absorbance*shading)))*shadow*lightdist(xdist,ydist,zdist)
                }
            }
        }
    }
    arr
}

# TODO: thorough unit test needed
#' Calculate point irradiance given some lamps, soft_surf, hard_surf, and a terrain raster
#'
#' @param lamps
#' @param soft_surf
#' @param hard_surf
#' @param terrain
#' @return point_irradiance raster RasterLayer
calc_point_irradiance <- function(lamps, soft_surf, hard_surf, terrain) {
    point_irradiance <- soft_surf
    raster::values(point_irradiance) <- 0
    ext <- 100
    for(lamp in 1:dim(lamps)[1]) {
        x <- lamps$x[lamp]
        y <- lamps$y[lamp]
        z <- lamps$z[lamp]

        # Get lit area indices
        lit_area <- cal_light_surface_indices(x, y, z, hard_surf, ext)
        ri_min <- lit_area$ri_min
        ri_max <- lit_area$ri_max
        cj_min <- lit_area$cj_min
        cj_max <- lit_area$cj_max
        ri_lamp <- lit_area$ri_lamp
        cj_lamp <- lit_area$cj_lamp
        nrows <- lit_area$nrows
        ncols <- lit_area$ncols

        ### extract terrain data for area of influence of light 
        lit_terrain_blocks <- get_blocks(hard_surf, soft_surf, terrain, ri_min, cj_min, ncols, nrows)
        hard_block <- lit_terrain_blocks$hard_block
        soft_block <- lit_terrain_blocks$soft_block
        terrain_block <- lit_terrain_blocks$terrain_block

        ### find values for irradiance on a horizontal plane and sphere
        # TODO: why only do this for 200x200? is this a temporary thing to only get simple squares?
        if (ncols==200 & nrows==200) {
            # print(paste("ri_lamp, cj_lamp, z, nrows, ncols, ext", ri_lamp, cj_lamp, z, nrows, ncols, ext))
            # print(paste(dim(terrain_block),":", max(terrain_block)))
            # print(paste(dim(hard_block),":", max(hard_block)))
            # print(paste(dim(soft_block),":", max(soft_block)))
            point_irrad <- cal_irradiance_arr(ri_lamp, cj_lamp, z, ncols, nrows, ext, terrain_block, hard_block, soft_block)
            point_values <- raster::raster(point_irrad, 
                                        xmn=raster::xFromCol(point_irradiance, cj_min),
                                        ymn=raster::yFromRow(point_irradiance, ri_max),
                                        xmx=xFromCol(point_irradiance, cj_max),
                                        ymx=yFromRow(point_irradiance, ri_min))
            raster::origin(point_values) <- origin(point_irradiance)
            point_irradiance <- raster::mosaic(point_irradiance,point_values,fun=sum)
            raster::removeTmpFiles()
        }
    }
    return(point_irradiance)
}

lightdist <- function(xdist,ydist,zdist,theta=NULL) {
    xyzdist <- sqrt(xdist^2+ydist^2+zdist^2)
    return(1/(xyzdist^2))
}

#' Calculate resistance caused by lamps
#'
#' @param lamps lamp raster
#' @param soft_surf soft surface raster
#' @param hard_surf hard surface raster
#' @param dtm lidar digital terrain map
#' @param ext extent (boundary)
#' @param resmax
#' @param xmax
#' @return resistance raster
cal_lamp_resistance <- function(lamps, soft_surf, hard_surf, dtm, ext, resmax, xmax) {

    if (dim(lamps)==0) {
        resistance <- soft_surf
        values(resistance) <- 1
        return(resistance)
    }
    point_irradiance <- calc_point_irradiance(lamps, soft_surf, hard_surf, dtm)
    resistance <- light_resistance(resmax, xmax, point_irradiance)
    return(resistance)
}

light_resistance <- function(Resmax, Xmax, rast) {
    rast[is.na(rast==TRUE)] <- 0
    MaxPI <- maxValue(rast)
    raster_resistance <- round(calc(rast, fun=function(PI) {((PI/MaxPI)^Xmax)*Resmax}) + 1, digits = 3)
    raster_resistance[is.na(raster_resistance) == TRUE] <- 1
    # writeRaster(raster_resistance, filename=outputfile, NAflag=-9999, overwrite=TRUE)
    raster_resistance
}


