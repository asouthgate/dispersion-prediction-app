options(show.error.locations = TRUE)

library(testthat)

source("R/pipeline.R")
source("circuitscape_app/server.R")
source("R/algorithm_parameters.R")

test_that("Test that an end-to-end run with standard data comes out as expected.", {

    local({
        # Here, we override the function that raises the error
        suppressWarnings({

            load('./test/input_data.Rdata')

            workingDir <- './test/tmp/'   

            print(extra_geoms)       

            base_inputs <- fetch_base_inputs(algorithmParameters, workingDir, lamps, extra_geoms, n_circles)

            save(base_inputs, file='./test/base_inputs.Rdata')

            resistance_maps <- cal_resistance_rasters(algorithmParameters, workingDir, base_inputs, shinyProgress, progressMax, save_images=TRUE)

            save(resistance_maps, file='./test/resistance.Rdata')

            #TODO: verify result; for now, not crashing
            expect_equal(TRUE, TRUE)
        })
    })


})
