library(testthat)
library(R6)
library(glue)
library(rpostgis)
library(sp)
library(mockr)

source("circuitscape_app/generate.R")
source("circuitscape_app/algorithm_parameters.R")

gls <- {}
gls.v <- 1
mock_db_raster <- function(tab, ext, database_host, database_name, database_port, database_user, database_password) {
    print("doing some fake call")
    rast <- raster::raster(ext=ext)
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
    angles <- (1:100) * 2 * pi / 100
    xvals <- c()
    yvals <- c()
    for (r in seq(1, 100, 10)) {
        xvals <- c(xvals, r * cos(angles))
        yvals <- c(yvals, (r*gls.c) * sin(angles))
    }
    coords <- data.frame(x=xvals, y=yvals)
    sp <- sp::SpatialPoints(coords)
    gls.c <<- gls.c + 1
    sp
}

test_that("Test that the generate function works", {

    local({
        # Here, we override the function that raises the error
        suppressWarnings({
            local_mock(read_db_vector=mock_db_vector)
            local_mock(read_db_raster=mock_db_raster)
            algorithm_parameters <- AlgorithmParameters$new(
                Roost$new(0, 0, 30),
                RoadResistance$new(buffer=5, resmax=10, xmax=5),
                RiverResistance$new(buffer=5, resmax=10, xmax=5),
                LandscapeResistance$new(resmax=10, xmax=5),
                LinearResistance$new(buffer=5, resmax=10, xmax=5, rankmax=10),
                LampResistance$new(resmax=10, xmax=5, ext=20)
            )

            x <- generate(
                algorithmParameters=algorithm_parameters,
                workingDir="./test/tmp",
                lightsFilename="./test/test_lights.csv",
                shinyProgress=NULL,
                verbose=TRUE,
                saveImages=FALSE
            )
        })
    })


})
