library(testthat)
library(R6)
library(glue)
library(rpostgis)
library(sp)
library(mockr)

source("circuitscape_app/pipeline.R")
source("circuitscape_app/server.R")
source("circuitscape_app/algorithm_parameters.R")

LOW_RESOLUTION = 5

gls <- {}
gls.v <- 1
mock_db_raster <- function(tab, ext, database_host, database_name, database_port, database_user, database_password) {
    print("doing some fake call")
    rast <- raster::raster(ext=ext, resolution=LOW_RESOLUTION)
    raster::values(rast) <- 1
    if (grepl("dtm", tab)) {
        raster::values(rast) <- 50 * seq_len(length(rast@data@values)) / length(rast@data@values)
    }
    else if (grepl("dsm", tab)) {
        raster::values(rast) <- 75 * seq_len(length(rast@data@values)) / length(rast@data@values)
    }
    else {
        raster::values(rast) <- seq_len(length(rast@data@values)) %% 10
    }
    for (j in seq(1, length(rast@data@values), 100)) {
        rast@data@values[j:j+100] <- NA
    }
    rast
}

gls.c <- 1
mock_db_vector <- function(roads_table, ext, database_host, database_name, database_port, database_user, database_password) {
    if (gls.c == 1) {
        xvals <- c(50, 100)
        yvals <- c(50, 100)
        # coords <- data.frame(x=xvals, y=yvals)
        coords <- matrix(c(xvals, yvals), ncol=2)
        sp <- raster::spLines(coords)
    }
    else if (gls.c == 2) {
        xvals <- 0:100
        yvals <- sin(xvals) * 5
        coords <- matrix(c(xvals, yvals), ncol=2)
        sp <- raster::spLines(coords)
    }
    else if (gls.c == 3) {
        xvals <- c(0, 10, 10, 0)
        yvals <- c(0, 0, 10, 10)
        coords <- matrix(c(xvals, yvals), ncol=2)
        sp <- raster::spPolygons(coords)

    }
    print(sp)
    gls.c <<- gls.c + 1
    sp
}

test_that("Test that the input generation function works", {

    local({
        # Here, we override the function that raises the error
        suppressWarnings({
            local_mock(read_db_vector=mock_db_vector)
            local_mock(read_db_raster=mock_db_raster)
            algorithm_parameters <- AlgorithmParameters$new(
                Roost$new(0, 0, 60),
                RoadResistance$new(buffer=5, resmax=10, xmax=5),
                RiverResistance$new(buffer=5, resmax=10, xmax=5),
                LandscapeResistance$new(resmax=10, xmax=5),
                LinearResistance$new(buffer=5, resmax=10, xmax=5, rankmax=10),
                LampResistance$new(resmax=10, xmax=5, ext=20),
                resolution = LOW_RESOLUTION
            )

            base_inputs <- fetch_base_inputs(algorithm_parameters, "./test/tmp", "./test/test_lights.csv", NULL)

            prepare_circuitscape_ini_file("./test/tmp")

            x <- generate(
                algorithmParameters=algorithm_parameters,
                workingDir="./test/tmp",
                base_inputs=base_inputs,
                shinyProgress=NULL,
                verbose=TRUE,
                saveImages=TRUE
            )

            #TODO: verify result; for now, not crashing
            expect_equal(TRUE, TRUE)
        })
    })


})
