library(raster)
library(logger)

source("circuitscape_app/irradiance.R")

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

    # TODO: + 1 should not be added, there should only be one + 1 done at the end, otherwise min will be 5 or something when it should be 1
    # If empty, resistance is min, which seems to be taken to be 1, since + 1 is added 
    if (length(roads)  == 0) {
        resistance <- groundrast
        values(resistance) <- 1
        terra::crs(resistance) <- terra::crs(groundrast)
        return(resistance)
    }

    road_distance <- cal_distance_raster(roads, groundrast)

    # resistance <- round(calc(road_distance,
    #                             function(d) {
    #                                 ifelse(d > buffer, 0, ( ((1 - (d/buffer))*0.5 + 0.5) ^ xmax) * resmax)
    #                             }
    #                         ) + 1,
    #                     digits=3)

    resistance <- calc(road_distance,
        function(d) {
            ifelse(d > buffer, 0, ( ((1 - (d/buffer))*0.5 + 0.5) ^ xmax) * resmax)
        }
    ) + 1


    return(resistance)
}

#' Calculate a resistance map for rivers
#'
#' @param river sp-class (SpatialPoints*, SpatialMultiPoints*, SpatialLines*, or SpatialPolygons*)
#' @param groundrast
#' @param buffer meters to river
#' @param resmax maximum resistance of "layers"
#' @param xmax "slope value"
#' @return raster::raster object for rivers
cal_river_resistance <- function(river, groundrast, buffer, resmax, xmax) {

    # TODO: find out why this was set to resmax, it should be resmax * xmax, if resistance increases the further away from the river, until 
    # rbuff = resmax
    # TODO: there is only one parameter needed, xmax * resmax, and rbuff, the resistance past the cutoff distance, should be the same as on the boundary
    rbuff <- resmax

    # If empty geom, resistance should be zero, for some reason it is given rbuff + 1, as in the calculation below
    if (length(river)  == 0) {
        resistance <- groundrast
        values(resistance) <- rbuff + 1
        return(resistance)
    }

    # TODO: Why is resmax set to 1 
    # resmax <- 1

    river_distance <- cal_distance_raster(river, groundrast)

    # no river -> zero resistance
    river_distance[is.na(river_distance)] <- 0

    resistance <- calc(river_distance,
                            function(d) {
                                # TODO: move to a separate function, since reused
                                # TODO: reference does not use a power
                                # ifelse(d > buffer, rbuff, ((d/buffer)^xmax)*resmax)
                                ifelse(d > buffer, rbuff, ((d/buffer) ^ xmax) * resmax)
                            }
                        ) + 1
    # TODO: why?
    resistance[is.na(resistance)] <- 1

    return(resistance)
}

#' Calculate a resistance map given distance rasters for features
#'
#' @param buffer distance from features
#' @param rankmax
#' @param rbuff
#' @param resmax
#' @param xmx
#' @param distance_rasters
#' @param effect can be "negative" or something else
#' @return resistance raster
distance2resistance <- function(buffer, rankmax, resmax, xmax, distance_rasters) {
    raster_resistance <- distance_rasters[[1, 1]]
    raster::values(raster_resistance) <- 0
    for (i in nrow(distance_rasters)) {

        rast <- distance_rasters[[i, 1]]
        Ranking <- distance_rasters[[i, 2]]
        rbuff <- (( (0.5 + 0.5 * (Ranking/rankmax))^ xmax) * resmax) + 1
        # rmin <- (0.5 * (((Ranking/rankmax) ^ xmax) * resmax)) + 1

        rast[is.na(rast) == TRUE] <- 0
        # partial_resistance <- round(raster::calc(rast, function(d) {ifelse(d > buffer, rbuff, (((0.5 * (d/buffer) + 0.5 * (Ranking/rankmax))^ xmax) * resmax))}) + 1, digits=3) 
        partial_resistance <- raster::calc(rast, function(d) {ifelse(d > buffer, rbuff, (((0.5 * (d/buffer) + 0.5 * (Ranking/rankmax))^ xmax) * resmax))}) + 1 

        raster_resistance <- raster::overlay(partial_resistance, raster_resistance, fun = max)

    }
    raster_resistance[is.na(raster_resistance) == TRUE] <- 1
    raster_resistance
}

cal_distance_with_defaults <- function(rast, max_d=999999999999) {
    logger::log_info("Calculating distance for feature raster (hedges, uhedges, trees)")

    rdist <- rast
    
    if (!(NA %in% rast@data@values)) {
        # everything is a hedge
        logger::log_warn("Surface is only hedges")
        values(rdist) <- 0

    } else if (!(1 %in% rast@data@values)) {
        # no hedges, everything max distance
        logger::log_warn("Surface contains no hedges")
        values(rdist) <- max_d

    } else {
        # some hedges, can cal dist
        rast <- raster::buffer(rast, width=10)
        rdist <- raster::distance(rast)
    }

    rdist
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
    mh_dist <- cal_distance_with_defaults(manhedge)


    unmanhedge <- surf
    unmanhedge[surf<6 & surf>3] <- 1
    unmanhedge[surf>=6] <- NA
    unmanhedge[surf<=3] <- NA
    umh_dist <- cal_distance_with_defaults(unmanhedge)

    tree <- surf
    tree[surf>=6] <- 1
    tree[surf<6] <- NA
    tree_dist <- cal_distance_with_defaults(tree)

    # TODO: I suspect this may be wrong -- should be raster raster raster 1 2 4 -- change back if so?
    # TODO: further, why do they have a different order to the order of calculation?
    logger::log_info("Calculating distances")
    # distance_rasters <- matrix(c(raster::distance(unmanhedge), 1, raster::distance(tree), 2, raster::distance(manhedge), 4), nrow=3, ncol=2)
    distance_rasters <- matrix(c(umh_dist, tree_dist, mh_dist, 1, 2, 4), nrow=3, ncol=2)
    return(distance_rasters)
}

# TODO: what are soft and hard surfaces?
#   Soft surf appears to have trees and hedges extracted later
#' Given a dtm, dsm raster and buildings vector, calculate surfaces, and soft and hard surfaces
calc_surfs <- function(dtm, dsm, buildings) {
    # Returns surf, soft and hard surface rasters
    logger::log_info("getting surf from dsm/dtm")
    surf <- dsm - dtm

    logger::log_info("getting soft surf")
    soft_surf <- (buildings+1) * surf
    soft_surf[is.na(soft_surf)] <- 0
    soft_surf <- surf - soft_surf

    logger::log_info("getting hard surf")
    hard_surf <- buildings
    hard_surf[is.na(hard_surf)] <- 1
    hard_surf <- hard_surf * surf
    hard_surf <- abs(hard_surf - surf)

    return(c("surf"=surf, "soft_surf"=soft_surf, "hard_surf"=hard_surf))
}

ranked_resistance <- function(conductance, Rankmax, Resmax, Xmax) {
    resistance <- raster::calc(conductance, fun=function(rank) {ifelse(rank == Rankmax, Resmax, ((rank/Rankmax) ^ Xmax) * Resmax)})
    resistance <- resistance + 1
    # resistance <- round(resistance, digits = 3)
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
get_landscape_resistance_lcm <- function(lcm, buildings, surfs, Rankmax, Resmax, Xmax) {
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
    # resistance <- ranked_resistance(conductance, Rankmax, Resmax, Xmax)
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
    logger::log_info("Preparing lidar rasters for linear resistance")
    distance_rasters <- prep_lidar_rasters(surf)

    logger::log_info("Converting distance to resistance")

    resistance <- distance2resistance(buffer, rankmax, resmax, xmax, distance_rasters)
    resistance
}

#' Given x, [lb, ub], return x if in [lb, ub], or lb, ub if <lb, )>ub
bound_val <- function(x, lb, ub) {
    v <- min(max(x, lb), ub)
    return(v)
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
    point_irradiance <- wrap_cal_irradiance(lamps, hard_surf, soft_surf, dtm)
    resistance <- light_resistance(resmax, xmax, point_irradiance)
    return(resistance)
}

light_resistance <- function(Resmax, Xmax, rast) {
    rast[is.na(rast==TRUE)] <- 0
    MaxPI <- maxValue(rast)
    # raster_resistance <- round(calc(rast, fun=function(PI) {((PI/MaxPI)^Xmax) * Resmax}) + 1, digits = 5)
    raster_resistance <- calc(rast, fun=function(PI) {((PI/MaxPI)^Xmax) * Resmax}) + 1
    raster_resistance[is.na(raster_resistance) == TRUE] <- 1
    # writeRaster(raster_resistance, filename=outputfile, NAflag=-9999, overwrite=TRUE)
    raster_resistance
}
