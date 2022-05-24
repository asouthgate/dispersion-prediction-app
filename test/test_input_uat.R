library(testthat)

source("circuitscape_app/generate.R")
source("circuitscape_app/server.R")
source("circuitscape_app/algorithm_parameters.R")

test_that("Test that an end-to-end run with standard data comes out as expected.", {

    local({
        # Here, we override the function that raises the error
        suppressWarnings({

            load(file="test/test_uat_input_data.RData")
            algorithmParameters$resolution <- 25
            algorithmParameters$roost$radius <- algorithmParameters$roost$radius * 2
            # First case is a location with data, lamps, drawn building shapes

            workingDir = "test/tmp/uat/1"
            dir.create(workingDir, recursive = TRUE)
            dir.create(paste0(workingDir, "/circuitscape"))

            prepare_circuitscape_ini_file(workingDir)
            logger::log_info(paste("workingDir is:", workingDir))

            base_inputs <- fetch_base_inputs(algorithmParameters, workingDir, lamps, extra_geoms)
            logger::log_info("Got base inputs.")

            resistance_maps <- cal_resistance_rasters(algorithmParameters, workingDir, base_inputs, shinyProgress, progressMax, verbose=TRUE, saveImages=TRUE)
            logger::log_info("Got resistance maps.")

            log_current_map <- call_circuitscape(workingDir, TRUE, TRUE)
            logger::log_info("Got current map.")

            # Second case, better parameters

            workingDir = "test/tmp/uat/2"
            dir.create(workingDir, recursive = TRUE)
            dir.create(paste0(workingDir, "/circuitscape"))

            prepare_circuitscape_ini_file(workingDir)
            logger::log_info(paste("workingDir is:", workingDir))

            algorithmParameters$lampResistance$resmax <- 100
            # algorithmParameters$linearResistance$resmax <- 100

            base_inputs <- fetch_base_inputs(algorithmParameters, workingDir, lamps, extra_geoms)
            logger::log_info("Got base inputs.")

            resistance_maps <- cal_resistance_rasters(algorithmParameters, workingDir, base_inputs, shinyProgress, progressMax, verbose=TRUE, saveImages=TRUE)
            logger::log_info("Got resistance maps.")

            log_current_map <- call_circuitscape(workingDir, TRUE, TRUE)
            logger::log_info("Got current map.")


            # Third case, no lamps, should not completely skew patterns

            workingDir = "test/tmp/uat/3"
            dir.create(workingDir, recursive = TRUE)
            dir.create(paste0(workingDir, "/circuitscape"))

            prepare_circuitscape_ini_file(workingDir)
            logger::log_info(paste("workingDir is:", workingDir))

            logger::log_info("Deleting lights")
            extra_geoms$extra_lights <- data.frame(x=c(), y=c(), z=c())

            base_inputs <- fetch_base_inputs(algorithmParameters, workingDir, lamps, extra_geoms)
            logger::log_info("Got base inputs.")

            resistance_maps <- cal_resistance_rasters(algorithmParameters, workingDir, base_inputs, shinyProgress, progressMax, verbose=TRUE, saveImages=TRUE)
            logger::log_info("Got resistance maps.")

            log_current_map <- call_circuitscape(workingDir, TRUE, TRUE)
            logger::log_info("Got current map.")

            expect_equal(TRUE, TRUE)
        })
    })
})