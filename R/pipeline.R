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

source("circuitscape_app/progress.R")

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
    logger::log_debug("Combining with extra_geoms:")
    new_geom <- geom
    if (!is.null(extra_geom)) {
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

#' Get inputs for raster pipeline from db, and combining with inputs
#'
#' @param algorithm_parameters an algorithm_parameters object
#' @param working_dir directory to save data to
#' @param lamps a csv file with lamp x, y, z vals
#' @param extra_geoms spatial data objects to combine with db outputs
#' @return list of data for input into resistance pipeline
fetch_base_inputs <- function(algorithm_parameters, working_dir, lamps, extra_geoms, n_circles) {

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
    resolution <- algorithm_parameters$resolution
    ext <- create_extent(algorithm_parameters$roost$x, algorithm_parameters$roost$y, algorithm_parameters$roost$radius)

    logger::log_info("Generating ground raster")
    groundrast <- create_ground_rast(algorithm_parameters$roost$x, algorithm_parameters$roost$y, algorithm_parameters$roost$radius, resolution)

    logger::log_info("Writing ground.asc")
    writeRaster(
        groundrast,
        paste0(working_dir, "/circuitscape/ground.asc"),
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
    
    logger::log_info("Getting extra height rasters for extra buildings...")
    # This is because, for the db buildings, height is obtained from lidar data, nothing exists for drawings
    extra_height <- get_extra_height_rasters(groundrast, extra_geoms$extra_buildings, extra_geoms$zvals$building)

    logger::log_info("Fetching dtm raster from db")
    dtm <- read_db_raster(dtm_table, ext, database_host, database_name, database_port, database_user, database_password)

    logger::log_info("Fetching dsm raster from db")
    dsm <- read_db_raster(dsm_table, ext, database_host, database_name, database_port, database_user, database_password)
    
    logger::log_info("Resampling dtm raster")
    r_dtm <- raster::resample(dtm, groundrast)

    logger::log_info("Resampling dsm raster")
    r_dsm <- raster::resample(dsm, groundrast)
    logger::log_info("Adding the extra height from drawings") 
    r_dsm <- r_dsm + extra_height

    logger::log_info("Fetching lcm raster from db")
    lcm <- read_db_raster(lcm_table, ext, database_host, database_name, database_port, database_user, database_password)
    lcm_r <- raster::resample(lcm, groundrast)

    logger::log_info("Combining extra lights if there are any.")
    if (length(extra_geoms$extra_lights) > 0) { lamps <- rbind(lamps, extra_geoms$extra_lights) }

    logger::log_info("Getting circles")
    circles <- create_circles(groundrast, algorithm_parameters$roost$x, algorithm_parameters$roost$y, algorithm_parameters$roost$radius, n_circles)

    logger::log_info("Getting a disk")
    disk <- create_disk_mask(groundrast, algorithm_parameters$roost$x, algorithm_parameters$roost$y, algorithm_parameters$roost$radius)
    print("BUILDINGS")
    print(buildings)


    return(list(ext=ext, groundrast=groundrast, rivers=rivers, roads=roads, 
            buildings=buildings, lamps=lamps, lcm_r=lcm_r, r_dtm=r_dtm, r_dsm=r_dsm,
            lamps=lamps, circles=circles, dtm=dtm, buildingsvec=buildingsvec, extra_height=extra_height, disk=disk))
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
    lamps <- read.csv(file=lights_fname, col.names=c("x", "y", "z"))
    colnames(lamps) <- c("x", "y", "z")
    lamps <- lamps[(lamps$x-x)^2 + (lamps$y-y)^2 < (radius+ext)^2, ]
}

#' Resistance pipeline: calculate resistance layers which will go into circuitscape
#'
#' @param algorithm_parameters an algorithm_parameters object
#' @param working_dir directory to save data to
#' @param base_inputs input data to the pipeline
#' @param shiny_progress progressbar
cal_resistance_rasters <- function(algorithm_parameters, working_dir, base_inputs, shiny_progress, progress_max=0, save_images=TRUE)  {

    # TODO: check folders exist

    ext <- base_inputs$ext
    groundrast <- base_inputs$groundrast
    rivers <- base_inputs$rivers
    roads <- base_inputs$roads 
    buildings <- base_inputs$buildings
    lamps <- base_inputs$lamps
    lcm_r <- base_inputs$lcm_r
    r_dtm <- base_inputs$r_dtm
    r_dsm <- base_inputs$r_dsm
    lamps <- base_inputs$lamps
    circles <- base_inputs$circles
    dtm <- base_inputs$dtm

    print("BUILDINGS")
    print(buildings)

    # taskProgress <- TaskProgress$new(shiny_progress, 17)
    # taskProgress$incrementProgress(100)

    logger::log_info("Calculating road resistance")
    roadRes <- cal_road_resistance(roads, groundrast, algorithm_parameters$roadResistance$buffer, 
                                algorithm_parameters$roadResistance$resmax, algorithm_parameters$roadResistance$xmax)

    logger::log_info("Calculating river resistance")
    riverRes <- cal_river_resistance(rivers, groundrast, algorithm_parameters$riverResistance$buffer,
                                algorithm_parameters$riverResistance$resmax, algorithm_parameters$riverResistance$xmax)

    # TODO: add in a test to make sure the dtm/dsm are not all zeros -- it will crash the later steps
    logger::log_info("Calculating surfaces")
    surfs <- calc_surfs(r_dtm, r_dsm, buildings)

    # TODO: EXTRACT -------- CALCULATE LANDSCAPE RESISTANCE MAPS
    logger::log_info("Calculating lcm resistance")
    print(lcm_r)
    print(buildings)
    print(surfs)
    landscapeRes <- get_landscape_resistance_lcm(lcm_r, buildings, surfs, algorithm_parameters$linearResistance$rankmax,
                                    algorithm_parameters$linearResistance$resmax, algorithm_parameters$linearResistance$xmax)

    logger::log_info("Calculating linear resistance")
    linearRes <- get_linear_resistance(surfs$soft_surf, algorithm_parameters$linearResistance$buffer, algorithm_parameters$linearResistance$rankmax,
                                    algorithm_parameters$linearResistance$resmax, algorithm_parameters$linearResistance$xmax)

    logger::log_info("Calculating lamp resistance")
    lampRes <- cal_lamp_resistance(lamps, surfs$soft_surf, surfs$hard_surf, dtm,
                            algorithm_parameters$lampResistance$ext, algorithm_parameters$lampResistance$resmax, algorithm_parameters$lampResistance$xmax)

    logger::log_info("Getting total resistance")
    # totalRes <- lampRes + roadRes + linearRes + riverRes + landscapeRes
    # totalRes <- lampRes + roadRes + linearRes + landscapeRes

    # TODO: TEMPORARY FIX: ADDRESS AT RASTERS: SHOULD NOT HAVE 1, FLOOR SHOULD BE ZERO FOR EACH LAYER, ADDED ONTO 1
    minlr <- min(values(lampRes))
    lampRes <- lampRes - minlr
    minlr <- min(values(linearRes))
    linearRes <- linearRes - minlr
    minlr <- min(values(riverRes))
    riverRes <- riverRes - minlr
    minlr <- min(values(roadRes))
    roadRes <- roadRes - minlr
    minlr <- min(values(landscapeRes))
    landscapeRes <- landscapeRes - minlr

    # Minimum resistancec is 1
    totalRes_unnorm <- lampRes + roadRes + riverRes + landscapeRes + linearRes + 1

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
        save_image(lamps, "lamps.png", working_dir)
        save_image(lampRes, "lampRes.png", working_dir)
        save_image(totalRes, "totalRes.png", working_dir)
        save_image(totalRes_unnorm, "totalRes_unnorm.png", working_dir)
        save_image(log(totalRes_unnorm), "log_totalRes_unnorm.png", working_dir)
        save_image(log(totalRes), "log_totalRes.png", working_dir)
        save_image(circles, "circles.png", working_dir)
    }

    return(list(road_res=roadRes, buildings=buildings, river_res=riverRes, landscape_res=landscapeRes, linear_res=linearRes, lamp_res=lampRes, total_res=totalRes))

}

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

    Sys.unsetenv("LD_LIBRARY_PATH")
    compute <- paste0("compute(\"", working_dir, "/cs.ini\")")
    call <- paste0("julia -e 'using Circuitscape; ", compute, "'")
    # inifile <- paste0(working_dir, "/cs.ini")
    # call <- paste("julia scripts/run_circuitscape.jl", inifile)
    system(call)

    current = raster(paste0(working_dir, "/circuitscape/cs_out_curmap.asc"))
    logCurrent = log(current + 1)
    writeRaster(
        logCurrent,
        paste0(working_dir, "/circuitscape/logCurrent.tif"),
        "GTiff",
        overwrite=TRUE
    )
    if (save_images) { 
        save_image(current, "current.png", working_dir)
        save_image(logCurrent, "logCurrent.png", working_dir)
    }
    return(logCurrent)

}
