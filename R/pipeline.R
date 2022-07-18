library(raster)
library(rpostgis)
library(glue)
library(R6)
library(logger)
library(terra)
terraOptions(datatype="FLT8S")

source("R/db.R")
source("R/transform.R")
source("R/rasterfunc.R")
source("R/resistance.R")

# The Circuitscape Julia function is parameterised by a .ini file
# that contains the paths of files required to perform the Circuitscape
# algorithm. These working files (including the .ini fie) are stored in a
# different randomly named folder for each use of the app. The file paths
# in the .ini must be customised to use the random working directory. We
# start with a template (cs.ini.template) and replace each occurence of
# WORKINGDIR with the working directory.
prepare_circuitscape_ini_file <- function(working_dir) {
    # Inject the working dir into the file ini template file
    template_filename <- "./R/cs.ini.template"
    template <- readChar(template_filename, file.info(template_filename)$size)
    output <- stringr::str_replace_all(template, "WORKINGDIR", working_dir)
    # Save the injected template in the working dir
    output_filename <- paste0(working_dir, "/cs.ini")
    output_file <- file(output_filename)
    logger::log_info(paste(working_dir, output_filename, output_file))
    logger::log_info(paste("Writing ini file to", output_file))
    writeLines(output, output_file)
    close(output_file)
}

#' Save some plottable data to a png
#'
#' @param data
#' @param fname
#' @param working_dir
save_image <- function(data, fname, working_dir) {
    savepath <- paste0(working_dir, "/images/", fname)
    logger::log_info(paste("Saving", savepath))
    tryCatch(
        {
            png(savepath)
            plot(data, axes=TRUE)
            dev.off()
        },
        error = function(err) {
            logger::log_warn(paste("Failed to plot and save:", err$message))
        }
    )
}

#' Check spatial points object is not empty
#'
#' @param spdf SpatialPoints or SpatialPointsDataFrame
#' @return bool
sp_not_empty <- function(df) {
    return(ifelse(length(df) > 0, TRUE, FALSE))
}

#' Log some warnings if a spatial points object is not as it should be
#'
#' @param tag a string tag for warning logging
#' @param spdf SpatialPoints or SpatialPointsDataFrame
log_vector_warnings <- function(tag, spdf) {
    if (!sp_not_empty(spdf)) {
        logger::log_warn(paste(tag, "has no rows!"))
    }
}

#' Add extra geoms to existing geoms
combine_extra_geoms <- function(geom, extra_geom) {
    logger::log_info("Combining with extra_geoms:")
    print(extra_geoms)
    new_geom <- geom
    if (!is.null(extra_geom)) {
        logger::log_debug(paste("Got:", length(extra_geom), "new geoms"))
        if (length(new_geom) > 0) {
            # Bind will only work if db has returned spatialpolygons instead of spatialpoints. What happens if it's both?
            new_geom <- raster::bind(new_geom, extra_geom)
        } else {
            new_geom <- extra_geom
        }
    }
    return(new_geom)
}

#' Squash vals into a range
squash_vals <- function(r) {
    nona <- values(r)[!is.na(values(r))]
    maxx <- max(nona)
    minx <- min(nona)
    a <- 1
    b <- 10000
    oldr <- maxx-minx
    newr <- b-a
    values(r) <- (((values(r) - minx) * newr) / oldr) + a
    r
}

#' Get vector inputs for raster pipeline from db, and combining with inputs
#'
#' @param algorithm_parameters an algorithm_parameters object
#' @param working_dir directory to save data to
#' @return list of data for input into resistance pipeline
fetch_vector_inputs <- function(algorithm_parameters, working_dir) {

    logger::log_info("Reading config")
    config <- configr::read.config("~/.bats.cfg")
    database_host <- config$database$host
    database_name <- config$database$name
    database_password <- config$database$password
    database_user <- config$database$user
    database_port <- config$database$port

    roads_table <- gsub("'", "", config$database$roads_table)
    rivers_table <- gsub("'", "", config$database$rivers_table)
    buildings_table <- gsub("'", "", config$database$buildings_table)
    ext <- algorithm_parameters$extent

    # logger::log_info("Creating extent")
    # ext <- create_extent(algorithm_parameters$roost$x, algorithm_parameters$roost$y, algorithm_parameters$roost$radius)

    logger::log_info("Fetching roads from database")
    roads <- read_db_vector(roads_table, ext, database_host, database_name, database_port, database_user, database_password)
    log_vector_warnings("roads", roads)

    logger::log_info("Fetching rivers from database")
    rivers <- read_db_vector(rivers_table, ext, database_host, database_name, database_port, database_user, database_password)
    log_vector_warnings("rivers", rivers)

    logger::log_info("Fetching buildings from database")
    buildingsvec <- read_db_vector(buildings_table, ext, database_host, database_name, database_port, database_user, database_password)
    log_vector_warnings("buildingsvec", buildingsvec)

    result <- list(rivers=rivers, roads=roads, buildingsvec=buildingsvec)

    return(result)
}

#' Get raster inputs for raster pipeline from db, and combining with inputs
#'
#' @param algorithm_parameters an algorithm_parameters object
#' @param working_dir directory to save data to
#' @return list of data for input into resistance pipeline
fetch_raster_inputs <- function(algorithm_parameters, groundrast, working_dir) {

    logger::log_info("Reading config")
    config <- configr::read.config("~/.bats.cfg")
    database_host <- config$database$host
    database_name <- config$database$name
    database_password <- config$database$password
    database_user <- config$database$user
    database_port <- config$database$port
    dtm_table <- gsub("'", "", config$database$dtm_table)
    dsm_table <- gsub("'", "", config$database$dsm_table)
    lcm_table <- gsub("'", "", config$database$lcm_table)
    ext <- algorithm_parameters$extent

    # logger::log_info("Creating extent")
    # ext <- create_extent(algorithm_parameters$roost$x, algorithm_parameters$roost$y, algorithm_parameters$roost$radius)

    # n_rows_res <- round(2 * algorithm_parameters$roost$radius / algorithm_parameters$resolution)
    resolution <- algorithm_parameters$resolution

    logger::log_info("Fetching dtm raster from db")
    zero_raster <- groundrast
    values(zero_raster) <- 0
    dtm_result <- read_db_raster_default(dtm_table, ext, database_host, database_name, 
                        database_port, database_user, database_password, zero_raster, resolution)
    dtm <- dtm_result$raster
    dtm_failed <- dtm_result$failflag

    logger::log_info("Fetching dsm raster from db")
    dsm_result <- read_db_raster_default(dsm_table, ext, database_host, database_name,
                        database_port, database_user, database_password, zero_raster, resolution)
    dsm <- dsm_result$raster
    dsm_failed <- dsm_result$failflag

    logger::log_info("Resampling dtm raster")
    r_dtm <- raster::resample(dtm, groundrast)

    logger::log_info("Resampling dsm raster")
    r_dsm <- raster::resample(dsm, groundrast)

    logger::log_info("Fetching lcm raster from db")
    lcm_result <- read_db_raster_default(lcm_table, ext, database_host, database_name,
                        database_port, database_user, database_password, zero_raster, resolution)
    lcm <- lcm_result$raster
    lcm_failed <- lcm_result$failflag
    lcm_r <- raster::resample(lcm, groundrast)

    raster_failed <- dsm_failed | dtm_failed | lcm_failed

    return(list(lcm_r=lcm_r, r_dtm=r_dtm, r_dsm=r_dsm, dsm=dsm, dtm=dtm, raster_failed=raster_failed))
}

#' Get inputs for raster pipeline from db, and combining with inputs
#'
#' @param algorithm_parameters an algorithm_parameters object
#' @param working_dir directory to save data to
#' @param lamps a csv file with lamp x, y, z vals
#' @param extra_geoms spatial data objects to combine with db outputs
#' @return list of data for input into resistance pipeline
postprocess_inputs <- function(algorithm_parameters, groundrast, vector_inputs, raster_inputs, working_dir, lamps, extra_geoms) {

    logger::log_info("Reading config")
    config <- configr::read.config("~/.bats.cfg")
    database_host <- config$database$host
    database_name <- config$database$name
    database_password <- config$database$password
    database_user <- config$database$user
    database_port <- config$database$port
    dtm_table <- gsub("'", "", config$database$dtm_table)
    dsm_table <- gsub("'", "", config$database$dsm_table)
    lcm_table <- gsub("'", "", config$database$lcm_table)
    roads_table <- gsub("'", "", config$database$roads_table)
    rivers_table <- gsub("'", "", config$database$rivers_table)
    buildings_table <- gsub("'", "", config$database$buildings_table)
    n_circles <- algorithm_parameters$n_circles

    rivers <- vector_inputs$rivers
    roads <- vector_inputs$roads
    buildingsvec <- vector_inputs$buildingsvec
    # dtm <- raster_inputs$dtm
    lcm_r <- raster_inputs$lcm_r
    r_dtm <- raster_inputs$r_dtm
    r_dsm <- raster_inputs$r_dsm

    print("INput processor got extra geoms:")
    print(extra_geoms)

    # logger::log_info("Creating extent")
    # ext <- create_extent(algorithm_parameters$roost$x, algorithm_parameters$roost$y, algorithm_parameters$roost$radius)

    # n_rows_res <- round(2 * algorithm_parameters$roost$radius / algorithm_parameters$resolution)
    # resolution <- algorithm_parameters$resolution

    # logger::log_info("Generating ground raster")
    # groundrast <- create_ground_rast(algorithm_parameters$roost$x, algorithm_parameters$roost$y, algorithm_parameters$roost$radius, resolution)

    logger::log_info("Writing ground.asc")
    writeRaster(
        groundrast,
        paste0(working_dir, "/circuitscape/ground.asc"),
        overwrite=TRUE
    ) # TODO: Create a random filename for each request

    # logger::log_info("Fetching roads from database")
    # roads <- read_db_vector(roads_table, ext, database_host, database_name, database_port, database_user, database_password)
    # log_vector_warnings("roads", roads)

    # logger::log_info("Fetching rivers from database")
    # rivers <- read_db_vector(rivers_table, ext, database_host, database_name, database_port, database_user, database_password)
    # log_vector_warnings("rivers", rivers)

    # logger::log_info("Fetching buildings from database")
    # buildingsvec <- read_db_vector(buildings_table, ext, database_host, database_name, database_port, database_user, database_password)
    # log_vector_warnings("buildingsvec", buildingsvec)

    logger::log_info("Combining extra building geoms if there are any.")
    buildingsvec <- combine_extra_geoms(buildingsvec, extra_geoms$extra_buildings)

    logger::log_info("Combining extra river geoms if there are any.")
    print(extra_geoms$extra_rivers)
    rivers <- combine_extra_geoms(rivers, extra_geoms$extra_rivers)

    logger::log_info("Combining extra road geoms if there are any.")
    roads <- combine_extra_geoms(roads, extra_geoms$extra_roads)

    logger::log_info("Rasterizing buildings")
    buildings <- rasterize_buildings(buildingsvec, groundrast)
    
    # logger::log_info("Getting extra height rasters for extra buildings...")
    # # This is because, for the db buildings, height is obtained from lidar data, nothing exists for drawings
    # extra_height <- get_extra_height_rasters(groundrast, extra_geoms$extra_buildings, extra_geoms$zvals$building)

    # logger::log_info("Fetching dtm raster from db")
    # zero_raster <- groundrast
    # values(zero_raster) <- 0
    # dtm_result <- read_db_raster_default(dtm_table, ext, database_host, database_name, 
    #                     database_port, database_user, database_password, zero_raster, n_rows_res)
    # dtm <- dtm_result$raster
    # dtm_failed <- dtm_result$failflag

    # logger::log_info("Fetching dsm raster from db")
    # dsm_result <- read_db_raster_default(dsm_table, ext, database_host, database_name,
    #                     database_port, database_user, database_password, zero_raster, n_rows_res)
    # dsm <- dsm_result$raster
    # dsm_failed <- dsm_result$failflag

    # logger::log_info("Resampling dtm raster")
    # r_dtm <- raster::resample(dtm, groundrast)

    # logger::log_info("Resampling dsm raster")
    # r_dsm <- raster::resample(dsm, groundrast)
    # logger::log_info("Adding the extra height from drawings")
    # r_dsm <- r_dsm + extra_height

    # logger::log_info("Fetching lcm raster from db")
    # lcm_result <- read_db_raster_default(lcm_table, ext, database_host, database_name, 
    #                     database_port, database_user, database_password, zero_raster, n_rows_res)
    # lcm <- lcm_result$raster
    # lcm_failed <- lcm_result$failflag
    # lcm_r <- raster::resample(lcm, groundrast)

    logger::log_info("Combining extra lights if there are any.")
    if (length(extra_geoms$extra_lights) > 0) {
        lamps <- rbind(lamps, extra_geoms$extra_lights)
    }

    logger::log_info("Getting circles")
    circles <- create_circles(groundrast, algorithm_parameters$roost$x, algorithm_parameters$roost$y, algorithm_parameters$roost$radius, n_circles)

    logger::log_info("Getting a disk")
    disk <- create_disk_mask(groundrast, algorithm_parameters$roost$x, algorithm_parameters$roost$y, algorithm_parameters$roost$radius)

    # raster_failed <- dsm_failed | dtm_failed | lcm_failed

    # TODO: could replace with a struct
    return(list(groundrast=groundrast, lcm_r=lcm_r, r_dtm=r_dtm, r_dsm=r_dsm, rivers=rivers, roads=roads,
            buildingsvec=buildingsvec, buildingsrast=buildings, lamps=lamps,
            lamps=lamps, circles=circles, disk=disk))
}

#' Load street lamp locations from a csv file, keep if within ext of the circle boundary
#'
#' @param lights_fname
#' @param x
#' @param y
#' @param ext=100
#' @param radius
#' @return dataframe with lights that are within a given circle
load_lamps <- function(lights_fname, x, y, radius, ext=100) {
    logger::log_info("Loading lamps...")
    lamps <- read.csv(file=lights_fname, col.names=c("x", "y", "z"))
    colnames(lamps) <- c("x", "y", "z")
    lamps <- lamps[(lamps$x-x)^2 + (lamps$y-y)^2 < (radius+ext)^2, ]
    logger::log_info("Lamps loaded!")
    lamps
}

#' Resistance pipeline: calculate resistance layers which will go into circuitscape
#'
#' @param algorithm_parameters an algorithm_parameters object
#' @param working_dir directory to save data to
#' @param base_inputs input data to the pipeline
cal_resistance_rasters <- function(algorithm_parameters, working_dir, base_inputs, save_images=TRUE)  {

    # TODO: check folders exist

    groundrast <- base_inputs$groundrast
    rivers <- base_inputs$rivers
    roads <- base_inputs$roads 
    buildings <- base_inputs$buildingsrast
    lamps <- base_inputs$lamps
    lcm_r <- base_inputs$lcm_r
    r_dtm <- base_inputs$r_dtm
    r_dsm <- base_inputs$r_dsm
    lamps <- base_inputs$lamps
    circles <- base_inputs$circles
    # dtm <- raster_inp$dtm

    logger::log_info("Calculating road resistance")
    roadRes <- cal_road_resistance(roads, groundrast, algorithm_parameters$roadResistance$buffer, 
                                algorithm_parameters$roadResistance$resmax, algorithm_parameters$roadResistance$xmax)

    logger::log_info("Calculating river resistance")
    riverRes <- cal_river_resistance(rivers, groundrast, algorithm_parameters$riverResistance$buffer,
                                algorithm_parameters$riverResistance$resmax, algorithm_parameters$riverResistance$xmax)

    logger::log_info("Calculating surfaces")
    print(buildings)
    surfs <- calc_surfs(r_dtm, r_dsm, buildings)

    logger::log_info("Calculating lcm resistance")
    landscapeRes <- get_landscape_resistance_lcm(lcm_r, buildings, surfs$soft_surf, algorithm_parameters$landscapeResistance$rankmax,
                                    algorithm_parameters$landscapeResistance$resmax, algorithm_parameters$landscapeResistance$xmax)

    logger::log_info("Calculating linear resistance")
    drl <- prep_lidar_rasters(surfs$soft_surf)
    distance_rasters <- drl$distance_rasters
    linearRes <- get_linear_resistance(distance_rasters, algorithm_parameters$linearResistance$buffer, algorithm_parameters$linearResistance$rankmax,
                                    algorithm_parameters$linearResistance$resmax, algorithm_parameters$linearResistance$xmax)

    logger::log_info("Calculating lamp irradiance")
    point_irradiance <- cal_lamp_irradiance(lamps, surfs$soft_surf, surfs$hard_surf, r_dtm, algorithm_parameters$lampResistance$ext)
    lampRes <- light_resistance(algorithm_parameters$lampResistance$resmax, algorithm_parameters$lampResistance$xmax, point_irradiance)

    logger::log_info("Getting total resistance")

    totalRes_unnorm <- lampRes + roadRes + riverRes + landscapeRes + linearRes
    # Make sure the minimum non-NA is 1
    totalRes_unnorm <- totalRes_unnorm + 1

    logger::log_info("Normalizing total resistance")
    # TODO: if there are buildings present, this doesnt seem to be required; it's because of range of values
    # squash between [1,100]
    totalRes <- squash_vals(totalRes_unnorm)

    # totalRes <- totalRes_unnorm

    logger::log_info("Got total resistance")

    logger::log_info("Writing resistance.asc")
    writeRaster(
        totalRes,
        paste0(working_dir, "/circuitscape/resistance.asc"),
        overwrite=TRUE
    )

    writeRaster(
        log(totalRes),
        paste0(working_dir, "/circuitscape/log_resistance.tif"),
        "GTiff",
        overwrite=TRUE
    )

    logger::log_info("Writing source.asc")
    writeRaster(
        circles,
        paste0(working_dir, "/circuitscape/source.asc"),
        NAflag=-9999,
        overwrite=TRUE
    )

    if (save_images) {
        logger::log_info("Saving images")
        dir.create(paste0(working_dir, "/images/"))

        save_image(groundrast, "groundrast.png", working_dir)
        save_image(roads, "roads.png", working_dir)
        save_image(rivers, "rivers.png", working_dir)
        save_image(buildings, "buildings.png", working_dir)
        save_image(landscapeRes, "landscapeRes.png", working_dir)
        save_image(linearRes, "linearRes.png", working_dir)
        save_image(lcm, "lcm.png", working_dir)
        save_image(lcm_r, "lcm_r.png", working_dir)
        save_image(roadRes, "roadRes.png", working_dir)
        save_image(riverRes, "riverRes.png", working_dir)
        save_image(log(point_irradiance), "logirradiance.png", working_dir)
        save_image(lamps, "lamps.png", working_dir)
        save_image(lampRes, "lampRes.png", working_dir)
        save_image(totalRes, "totalRes.png", working_dir)
        save_image(totalRes_unnorm, "totalRes_unnorm.png", working_dir)
        save_image(log(totalRes_unnorm), "log_totalRes_unnorm.png", working_dir)
        save_image(log(totalRes), "log_totalRes.png", working_dir)
        save_image(circles, "circles.png", working_dir)
    }

    return(list(road_res=roadRes, buildings=buildings, river_res=riverRes, 
                landscape_res=landscapeRes, manhedge=drl$manhedge, unmanhedge=drl$unmanhedge, tree=drl$tree,
                linear_res=linearRes, lamp_res=lampRes, 
                total_res=totalRes, soft_surf=surfs$soft_surf, hard_surf=surfs$hard_surf, 
                log_point_irradiance=log(point_irradiance)))

}

# submit_preprocess_pipeline <- function(input_data_fname) {
#     system(paste("srun Rscript scripts/run_preprocess_pipeline.R", input_data_fname)) 
# }

submit_resistance_pipeline <- function(input_data_fname) {
    system(paste("srun Rscript scripts/run_resistance_pipeline.R", input_data_fname)) 
}

submit_circuitscape <- function(input_working_dir) {
    system(paste("srun Rscript scripts/run_circuitscape.R", input_working_dir))
} 

#' Call circuitscape given a working directory with inputs
#' 
#' @param working_dir 
#' @param save_images bool
call_circuitscape <- function(working_dir, save_images) {

    # Create the call string
    Sys.unsetenv("LD_LIBRARY_PATH")
    compute <- paste0("compute(\"", working_dir, "/cs.ini\")")
    call <- paste0("julia -e 'using Circuitscape; ", compute, "'")

    system(call)

    current = raster(paste0(working_dir, "/circuitscape/cs_out_curmap.asc"))
    logCurrent = log(current + 1)
    
    writeRaster(
        logCurrent,
        paste0(working_dir, "/circuitscape/log_current.tif"),
        "GTiff",
        overwrite=TRUE
    )

    if (save_images) { 
        save_image(current, "current.png", working_dir)
        save_image(logCurrent, "logCurrent.png", working_dir)
    }

    return(logCurrent)

}
