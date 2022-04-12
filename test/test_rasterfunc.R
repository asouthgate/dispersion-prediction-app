library(testthat)
library(raster)

source("circuitscape_app/rasterfunc.R")

test_that("Test create ground rast functions", {

    groundrast <- create_ground_rast(0, 0, 1)
    expect_equal(groundrast@ncols, 200)
    expect_equal(groundrast@nrows, 200)

})
#> Passed ground rast test! 🥳
