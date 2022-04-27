options(show.error.locations = TRUE)

library(testthat)

source("circuitscape_app/generate.R")
source("circuitscape_app/server.R")
source("circuitscape_app/algorithm_parameters.R")

test_that("Test that an end-to-end run with standard data comes out as expected.", {

    local({
        # Here, we override the function that raises the error
        suppressWarnings({
            
            algorithm_parameters <- AlgorithmParameters$new(
                Roost$new(397185.752, 87860.641, 300),
                RoadResistance$new(buffer=200, resmax=10, xmax=5),
                RiverResistance$new(buffer=10, resmax=2000, xmax=4),
                LandscapeResistance$new(resmax=100, xmax=5),
                LinearResistance$new(buffer=10, resmax=22000, xmax=4, rankmax=3),
                LampResistance$new(resmax=100000000, xmax=1, ext=100),
                resolution = 1
            )

            prepare_circuitscape_ini_file("./test/tmp")

            x <- generate(
                algorithmParameters=algorithm_parameters,
                workingDir="./test/tmp",
                lightsFilename="./test/test_arne_lamps.csv",
                shinyProgress=NULL,
                verbose=TRUE,
                saveImages=TRUE
            )

            #TODO: verify result; for now, not crashing
            expect_equal(TRUE, TRUE)
        })
    })


})
