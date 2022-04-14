library(raster)
library(rpostgis)
library(glue)
library(R6)

source("circuitscape_app/db.R")
source("circuitscape_app/transform.R")
source("circuitscape_app/rasterfunc.R")
source("circuitscape_app/progress.R")

# TODO: why is ext 100 
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

generate <- function(algorithmParameters, workingDir, lightsFilename, shinyProgress, progressMax=0, verbose=TRUE, saveImages=FALSE) {

    # TODO: check folders exist

    # TODO: EXTRACT -------- GET CONFIG AND SETUP

    # taskProgress <- TaskProgress$new(shinyProgress, 17)
    # taskProgress$incrementProgress(100)

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


    if (verbose) {
        print("Generating with:")
        print(glue("roost=({algorithmParameters$roost$x}, {algorithmParameters$roost$y}); radius={algorithmParameters$roost$radius}m; lightsFilename={lightsFilename}"))
    }

    # TODO: EXTRACT -------- GET EXTENT AND GROUND RASTER
    # TODO: what is resolution
    resolution <- 1
    ext <- create_extent(algorithmParameters$roost$x, algorithmParameters$roost$y, algorithmParameters$roost$radius)

    # Ground Raster
    groundrast <- create_ground_rast(algorithmParameters$roost$x, algorithmParameters$roost$y, algorithmParameters$roost$radius, resolution)

    # TODO: necessary? used for?
    writeRaster(
        groundrast,
        paste0(workingDir, "/circuitscape/ground.asc"),
        overwrite=TRUE
    ) # TODO: Create a random filename for each request

    # TODO: If lots of logging needed, extract
    roads <- read_db_vector(roads_table, ext, database_host, database_name, database_port, database_user, database_password)
    rivers <- read_db_vector(rivers_table, ext, database_host, database_name, database_port, database_user, database_password)
    buildings <- read_db_vector(buildings_table, ext, database_host, database_name, database_port, database_user, database_password)

    # TODO: why?
    if (saveImages) {
        png("images/roads.png")
        plot(roads, axes=TRUE)
        png("images/rivers.png")
        plot(rivers, axes=TRUE)
        png("images/buildings.png")
        plot(buildings, axes=TRUE)
    }

    # TODO: EXTRACT -------- RASTERIZE BUILDINGS

    buildings <- raster::rasterize(buildings, groundrast)
    buildings[!is.na(buildings)] <- 1

    # TODO: EXTRACT -------- GET RESISTANCE MAPS FOR ROADS AND RIVERS
    # TODO: Why not for buildings?

    roadRes <- cal_road_resistance(roads, groundrast, algorithmParameters$roadResistance$buffer, 
                                algorithmParameters$roadResistance$resmax, algorithmParameters$roadResistance$xmax)
    riverRes <- cal_river_resistance(rivers, groundrast, algorithmParameters$riverResistance$buffer,
                                algorithmParameters$riverResistance$resmax, algorithmParameters$riverResistance$xmax)

    if (saveImages) {
         png("images/rasterizedBuildings.png")
         plot(buildings, axes=TRUE) 
         png("images/roadRes.png")
         plot(roadRes, axes=TRUE)
         png("images/riverRes.png")
         plot(riverRes, axes=TRUE)
    }

    # TODO: EXTRACT -------- GET LIDAR DATA AND PROCESS IT

    if (verbose) print("Querying LIDAR rasters...");

    dtm <- read_db_raster(dtm_table, ext, database_host, database_name, database_port, database_user, database_password)
    dsm <- read_db_raster(dsm_table, ext, database_host, database_name, database_port, database_user, database_password)

    if (saveImages) {
        png("images/dtm.png")
        plot(dtm, axes=TRUE)
        png("images/dsm.png")
        plot(dsm, axes=TRUE)
    }

    r_dtm <- raster::resample(dtm, groundrast)
    r_dsm <- raster::resample(dsm, groundrast)

    # TODO: EXTRACT -------- CALCULATE SURFACES
    # TODO: calculate what surfaces?

    # TODO: add in a guard to make sure the dtm/dsm are not all zeros -- it will crash the later steps
    surfs <- calc_surfs(r_dtm, r_dsm, buildings)
    # TODO: what is LCM raster?

    lcm <- read_db_raster(lcm_table, ext, database_host, database_name, database_port, database_user, database_password)
    lcm_r <- raster::resample(lcm, groundrast)

    if (saveImages) { 
        png("images/lcm.png");
        plot(lcm, axes=TRUE);
        png("images/lcm_r.png");
        plot(lcm_r, axes=TRUE)
    }

    # TODO: EXTRACT -------- CALCULATE LANDSCAPE RESISTANCE MAPS

    landscapeRes <- get_landscape_resistance_lcm(lcm_r, buildings, surfs)
    linearRes <- get_linear_resistance(surfs$soft_surf, algorithmParameters$linearResistance$buffer, algorithmParameters$linearResistance$rankmax,
                                    algorithmParameters$linearResistance$resmax, algorithmParameters$linearResistance$xmax)

    if (saveImages) {
        png("images/landscapeRes.png")
        plot(landscapeRes, axes=TRUE)
        png("images/linearRes.png")
        plot(linearRes, axes=TRUE)
    }

    # TODO: EXTRACT -------- CALCULATE LIGHT DATA RESISTANCE MAPS

    lamps <- load_lamps(lightsFilename, algorithmParameters$roost$x, algorithmParameters$roost$y, algorithmParameters$roost$radius)
    lampRes <- cal_lamp_resistance(lamps, surfs$soft_surf, surfs$hard_surf, dtm,
                            algorithmParameters$lampResistance$ext, algorithmParameters$lampResistance$resmax, algorithmParameters$lampResistance$xmax)
    totalRes <- lampRes + roadRes + linearRes + riverRes + landscapeRes
    circles <- create_circles(groundrast, algorithmParameters$roost$x, algorithmParameters$roost$y, algorithmParameters$roost$radius)

    writeRaster(
        totalRes,
        paste0(workingDir, "/circuitscape/resistance.asc"),
        overwrite=TRUE
    )
    writeRaster(
        circles,
        paste0(workingDir, "/circuitscape/source.asc"),
        NAflag=-9999,
        overwrite=TRUE
    )

    if (saveImages) { 
        png("images/lampRes.png") 
        plot(lampRes, axes=TRUE) 
        png("images/lamps.png")
        plot(lamps$x,lamps$y, axes=TRUE) 
        png("images/totalRes.png")
        plot(totalRes, axes=TRUE)
        png("images/circles.png")
        plot(circles, axes=TRUE)
    }

    # TODO: EXTRACT -------- CALL JULIA

    # TODO: make this just a dependency
    julia_install_package_if_needed("Circuitscape") # if you don't already have the package installed
    julia_library("Circuitscape")                   # make sure Circuitscape is available
    julia_call(
        "compute",
        paste0(workingDir, "/cs.ini"),
        need_return="None"
    )

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
        png("images/current.png") 
        plot(current, axes=TRUE) 
        png("images/logCurrent.png")
        plot(logCurrent, axes=TRUE) 
    }

    #taskProgress$finalizeProgress()
}

# generate(
#     roost=c(274257,66207),
#     radius=300,
#     lightsFilename="gis-layers/lights.csv",
#     shinyProgress=NULL,
#     verbose=TRUE,
#     saveImages=FALSE
# )
