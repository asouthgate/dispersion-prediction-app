library(testthat)
library(mockery)
library(R6)
library(raster)

source("circuitscape_app/rasterfunc.R")
source("circuitscape_app/pipeline.R")
source("circuitscape_app/algorithm_parameters.R")
source("test/old_rasterfunc.R")

source("circuitscape_app/server.R")

test_that("Calculating point irradiance is fast enough", {

    # x <- 600
    # y <- 600
    # z <- 10

    ## ... code here ... ##

    rast <- raster::raster(xmn=300, xmx=1300, ymn=300, ymx=1300, resolution=1)

    print(rast)

    # zero resistance raster, should have simple illumination
    values(rast) <- 0
    hard_surf <- rast
    values(hard_surf) <- NA
    soft_surf <- rast
    values(soft_surf) <- pmax(rnorm(mean=100, n=length(values(hard_surf)), sd=50), 0)
    terrain <- rast

    nlights <- 10

    lampsdf <- data.frame(x=rnorm(mean=600, n=nlights, sd=100), y=rnorm(mean=600, n=nlights, sd=100), z=rnorm(mean=50, n=1, sd=5))

    start <- Sys.time()
    old_point_irradiance <- calc_point_irradiance_old(lampsdf, soft_surf, hard_surf, terrain)
    end <- Sys.time()
    
    print(end - start)

    expect_false(mean(values(old_point_irradiance)) == 0)

    point_irradiance <- calc_point_irradiance(lampsdf, soft_surf, hard_surf, terrain)
    expect_false(mean(values(point_irradiance)) == 0)

    print(mean(values(point_irradiance)))
    expect_equal(values(point_irradiance), values(old_point_irradiance))



})

LOW_RESOLUTION = 1

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


            prepare_circuitscape_ini_file("./test/tmp")

            t1 <- Sys.time()
            base_inputs <- fetch_base_inputs(algorithm_parameters, "./test/tmp", "./test/test_lights.csv", NULL)
            t2 <- Sys.time()

            x <- generate(
                algorithmParameters=algorithm_parameters,
                workingDir="./test/tmp",
                base_inputs=base_inputs,
                shinyProgress=NULL,
                verbose=TRUE,
                saveImages=TRUE
            )
            t3 <- Sys.time()
            print(t2-t1)
            print(t3-t2)

            #TODO: verify result; for now, not crashing
            expect_equal(TRUE, TRUE)
        })
    })


})
