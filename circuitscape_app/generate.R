library(raster)
library(rpostgis)
library(glue)
library(R6)
library(logger)

source("circuitscape_app/db.R")
source("circuitscape_app/transform.R")
source("circuitscape_app/rasterfunc.R")
source("circuitscape_app/progress.R")

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
    logger::log_debug("Combining with extra_geoms:")
    print(geom)
    print(extra_geom)
    new_geom <- geom
    if (!is.null(extra_geom)) {
        if (length(new_geom) > 0) {
            # Bind will only work if db has returned spatialpolygons instead of spatialpoints. What happens if it's both?
            new_geom <- raster::bind(new_geom, extra_geom)
        } else {
            new_geom <- extra_geom
        }
    }
    print(new_geom)
    return(new_geom)
}

fetch_base_inputs <- function(algorithmParameters, workingDir, lightsFilename, extra_geoms) {

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

    logger::log_info("Creating extent")
    resolution <- algorithmParameters$resolution
    ext <- create_extent(algorithmParameters$roost$x, algorithmParameters$roost$y, algorithmParameters$roost$radius)

    logger::log_info("Generating ground raster")
    groundrast <- create_ground_rast(algorithmParameters$roost$x, algorithmParameters$roost$y, algorithmParameters$roost$radius, resolution)

    logger::log_info("Writing ground.asc")
    writeRaster(
        groundrast,
        paste0(workingDir, "/circuitscape/ground.asc"),
        overwrite=TRUE
    ) # TODO: Create a random filename for each request

    logger::log_info("Fetching roads from database")
    roads <- read_db_vector(roads_table, ext, database_host, database_name, database_port, database_user, database_password)
    log_vector_warnings("roads", roads)

    logger::log_info("Fetching rivers from database")
    rivers <- read_db_vector(rivers_table, ext, database_host, database_name, database_port, database_user, database_password)
    log_vector_warnings("rivers", rivers)

    logger::log_info("Fetching buildings from database")
    buildingsvec <- read_db_vector(buildings_table, ext, database_host, database_name, database_port, database_user, database_password)
    log_vector_warnings("buildingsvec", buildingsvec)

    logger::log_info("Combining extra building geoms if there are any.")
    buildingsvec <- combine_extra_geoms(buildingsvec, extra_geoms$extra_buildings)

    logger::log_info("Combining extra river geoms if there are any.")
    rivers <- combine_extra_geoms(rivers, extra_geoms$extra_rivers)

    logger::log_info("Combining extra road geoms if there are any.")
    roads <- combine_extra_geoms(roads, extra_geoms$extra_roads)

    logger::log_info("Rasterizing buildings")
    buildings <- rasterize_buildings(buildingsvec, groundrast)

    logger::log_info("Fetching dtm raster from db")
    dtm <- read_db_raster(dtm_table, ext, database_host, database_name, database_port, database_user, database_password)

    logger::log_info("Fetching dsm raster from db")
    dsm <- read_db_raster(dsm_table, ext, database_host, database_name, database_port, database_user, database_password)

    logger::log_info("Resampling dtm raster")
    r_dtm <- raster::resample(dtm, groundrast)

    logger::log_info("Resampling dsm raster")
    r_dsm <- raster::resample(dsm, groundrast)

    logger::log_info("Fetching lcm raster from db")
    lcm <- read_db_raster(lcm_table, ext, database_host, database_name, database_port, database_user, database_password)
    lcm_r <- raster::resample(lcm, groundrast)

    logger::log_info("Loading lamps")
    lamps <- load_lamps(lightsFilename, algorithmParameters$roost$x, algorithmParameters$roost$y, algorithmParameters$roost$radius)

    logger::log_info("Combining extra lights if there are any.")
    if (length(extra_geoms$extra_lights) > 0) { lamps <- rbind(lamps, extra_geoms$extra_lights) }

    logger::log_info("Getting circles")
    circles <- create_circles(groundrast, algorithmParameters$roost$x, algorithmParameters$roost$y, algorithmParameters$roost$radius)

    logger::log_info("Getting a disk")
    disk <- create_disk_mask(groundrast, algorithmParameters$roost$x, algorithmParameters$roost$y, algorithmParameters$roost$radius)

    return(list(ext=ext, groundrast=groundrast, rivers=rivers, roads=roads, 
            buildings=buildings, lamps=lamps, lcm_r=lcm_r, r_dtm=r_dtm, r_dsm=r_dsm,
            lamps=lamps, circles=circles, dtm=dtm, buildingsvec=buildingsvec, disk=disk))
}

#! QUESTION
# TODO: why is ext 100, why do we have radius + extent?
#' Load street lamp locations from a csv file
#'
#' @param lights_fname
#' @param x
#' @param y
#' @param ext=100
#' @param radius
#' @return dataframe with lights that are within a given circle
load_lamps <- function(lights_fname, x, y, radius, ext=100) {
    lamps <- read.csv(file=lights_fname, col.names=c("x", "y", "z"))
    colnames(lamps) <- c("x", "y", "z")
    lamps <- lamps[(lamps$x-x)^2 + (lamps$y-y)^2 < (radius+ext)^2,]
}

cal_resistance_rasters <- function(algorithmParameters, workingDir, base_inputs, shinyProgress, progressMax=0, verbose=TRUE, saveImages=TRUE)  {

    # TODO: check folders exist

    ext=base_inputs$ext
    groundrast=base_inputs$groundrast
    rivers=base_inputs$rivers
    roads=base_inputs$roads 
    buildings=base_inputs$buildings
    lamps=base_inputs$lamps
    lcm_r=base_inputs$lcm_r
    r_dtm=base_inputs$r_dtm
    r_dsm=base_inputs$r_dsm
    lamps=base_inputs$lamps
    circles=base_inputs$circles
    dtm=base_inputs$dtm

    # taskProgress <- TaskProgress$new(shinyProgress, 17)
    # taskProgress$incrementProgress(100)

    # logger::log_info("Reading config")
    # config <- configr::read.config("~/.bats.cfg")
    # database_host <- config$database$host
    # database_name <- config$database$name
    # database_password <- config$database$password
    # database_user <- config$database$user
    # database_port <- config$database$port
    # dtm_table <- gsub("'", "", config$database$dtm_table)
    # dsm_table <- gsub("'", "", config$database$dsm_table)
    # lcm_table <- gsub("'", "", config$database$lcm_table)
    # roads_table <- gsub("'", "", config$database$roads_table)
    # rivers_table <- gsub("'", "", config$database$rivers_table)
    # buildings_table <- gsub("'", "", config$database$buildings_table)

    # # TODO: what is resolution
    # logger::log_info("Creating extent")
    # resolution <- algorithmParameters$resolution
    # ext <- create_extent(algorithmParameters$roost$x, algorithmParameters$roost$y, algorithmParameters$roost$radius)

    # # Ground Raster
    # logger::log_info("Generating ground raster")
    # groundrast <- create_ground_rast(algorithmParameters$roost$x, algorithmParameters$roost$y, algorithmParameters$roost$radius, resolution)

    # logger::log_info("Writing ground.asc")
    # writeRaster(
    #     groundrast,
    #     paste0(workingDir, "/circuitscape/ground.asc"),
    #     overwrite=TRUE
    # ) # TODO: Create a random filename for each request

    # logger::log_info("Fetching roads from database")
    # roads <- read_db_vector(roads_table, ext, database_host, database_name, database_port, database_user, database_password)
    # log_vector_warnings("roads", roads)

    # logger::log_info("Fetching rivers from database")
    # rivers <- read_db_vector(rivers_table, ext, database_host, database_name, database_port, database_user, database_password)
    # log_vector_warnings("rivers",rivers)

    # logger::log_info("Fetching buildings from database")
    # buildings <- read_db_vector(buildings_table, ext, database_host, database_name, database_port, database_user, database_password)
    # log_vector_warnings("buildings", buildings)

    # logger::log_info("Rasterizing buildings")
    # buildings <- rasterize_buildings(buildings, groundrast)

    # TODO: EXTRACT -------- GET RESISTANCE MAPS FOR ROADS AND RIVERS
    # TODO: Why not for buildings?

    logger::log_info("Calculating road resistance")
    roadRes <- cal_road_resistance(roads, groundrast, algorithmParameters$roadResistance$buffer, 
                                algorithmParameters$roadResistance$resmax, algorithmParameters$roadResistance$xmax)

    logger::log_info("Calculating river resistance")
    riverRes <- cal_river_resistance(rivers, groundrast, algorithmParameters$riverResistance$buffer,
                                algorithmParameters$riverResistance$resmax, algorithmParameters$riverResistance$xmax)



    # TODO: EXTRACT -------- GET LIDAR DATA AND PROCESS IT

    # logger::log_info("Fetching dtm raster from db")
    # dtm <- read_db_raster(dtm_table, ext, database_host, database_name, database_port, database_user, database_password)

    # logger::log_info("Fetching dsm raster from db")
    # dsm <- read_db_raster(dsm_table, ext, database_host, database_name, database_port, database_user, database_password)

    # logger::log_info("Resampling dtm raster")
    # r_dtm <- raster::resample(dtm, groundrast)

    # logger::log_info("Resampling dsm raster")
    # r_dsm <- raster::resample(dsm, groundrast)

    # TODO: EXTRACT -------- CALCULATE SURFACES
    # TODO: calculate what surfaces?

    # TODO: add in a test to make sure the dtm/dsm are not all zeros -- it will crash the later steps
    logger::log_info("Calculating surfaces")
    surfs <- calc_surfs(r_dtm, r_dsm, buildings)
    # TODO: what is LCM raster?

    # logger::log_info("Fetching lcm raster from db")
    # lcm <- read_db_raster(lcm_table, ext, database_host, database_name, database_port, database_user, database_password)
    # lcm_r <- raster::resample(lcm, groundrast)

    # TODO: EXTRACT -------- CALCULATE LANDSCAPE RESISTANCE MAPS
    logger::log_info("Calculating lcm resistance")
    landscapeRes <- get_landscape_resistance_lcm(lcm_r, buildings, surfs)

    logger::log_info("Calculating linear resistance")
    linearRes <- get_linear_resistance(surfs$soft_surf, algorithmParameters$linearResistance$buffer, algorithmParameters$linearResistance$rankmax,
                                    algorithmParameters$linearResistance$resmax, algorithmParameters$linearResistance$xmax)

    # TODO: EXTRACT -------- CALCULATE LIGHT DATA RESISTANCE MAPS

    # logger::log_info("Loading lamps")
    # lamps <- load_lamps(lightsFilename, algorithmParameters$roost$x, algorithmParameters$roost$y, algorithmParameters$roost$radius)

    logger::log_info("Calculating lamp resistance")
    lampRes <- cal_lamp_resistance(lamps, surfs$soft_surf, surfs$hard_surf, dtm,
                            algorithmParameters$lampResistance$ext, algorithmParameters$lampResistance$resmax, algorithmParameters$lampResistance$xmax)

    logger::log_info("Getting total resistance")
    totalRes <- lampRes + roadRes + linearRes + riverRes + landscapeRes

    # logger::log_info("Getting circles")
    # circles <- create_circles(groundrast, algorithmParameters$roost$x, algorithmParameters$roost$y, algorithmParameters$roost$radius)

    logger::log_info("Writing resistance.asc")
    writeRaster(
        totalRes,
        paste0(workingDir, "/circuitscape/resistance.asc"),
        overwrite=TRUE
    )

    logger::log_info("Writing source.asc")
    writeRaster(
        circles,
        paste0(workingDir, "/circuitscape/source.asc"),
        NAflag=-9999,
        overwrite=TRUE
    )

    if (saveImages) {
        logger::log_info("Saving images")
        dir.create(paste0(workingDir, "/images/"))

        save_image(groundrast, "groundrast.png", workingDir)
        save_image(roads, "roads.png", workingDir)
        save_image(rivers, "rivers.png", workingDir)
        save_image(buildings, "buildings.png", workingDir)
        save_image(landscapeRes, "landscapeRes.png", workingDir)
        save_image(linearRes, "linearRes.png", workingDir)
        save_image(lcm, "lcm.png", workingDir)
        save_image(lcm_r, "lcm_r.png", workingDir)
        save_image(roadRes, "roadRes.png", workingDir)
        save_image(riverRes, "riverRes.png", workingDir)
        save_image(lamps, "lamps.png", workingDir)
        save_image(lampRes, "lampRes.png", workingDir)
        save_image(totalRes, "totalRes.png", workingDir)
        save_image(circles, "circles.png", workingDir)
    }

}

generate <- function(algorithmParameters, workingDir, base_inputs, shinyProgress, progressMax=0, verbose=TRUE, saveImages=TRUE) {

    # base_inputs <- fetch_base_inputs(algorithmParameters, workingDir, lightsFilename)

    cal_resistance_rasters(algorithmParameters, workingDir, base_inputs, shinyProgress, progressMax, verbose, saveImages)

    # TODO: EXTRACT -------- CALL JULIA

    # TODO: make this just a dependency
    # julia_install_package_if_needed("Circuitscape") # if you don't already have the package installed
    # julia_library("Circuitscape")                   # make sure Circuitscape is available
    # julia_call(
    #     "compute",
    #     paste0(workingDir, "/cs.ini"),
    #     need_return="None"
    # )
    # tmp bugfix; precedence works badly with libcurl
    Sys.unsetenv("LD_LIBRARY_PATH")
    compute <- paste0("compute(\"", workingDir, "/cs.ini\")")
    call <- paste0("julia -e 'using Circuitscape; ", compute, "'")
    print(call)
    system(call)

    if (verbose) { print("Generating current raster...") }
    current = raster(paste0(workingDir, "/circuitscape/cs_out_curmap.asc"))
    logCurrent = log(current)
    writeRaster(
        logCurrent,
        paste0(workingDir, "/circuitscape/logCurrent.tif"),
        "GTiff",
        overwrite=TRUE
    )
    if (saveImages) { 
        png(paste0(workingDir, "/images/current.png"))
        plot(current, axes=TRUE) 
        png(paste0(workingDir, "/images/logCurrent.png"))
        plot(logCurrent, axes=TRUE) 
    }

    #taskProgress$finalizeProgress()
}