library(testthat)
library(R6)
library(glue)
library(rpostgis)
library(sp)
library(mockr)

source("circuitscape_app/generate.R")
source("circuitscape_app/algorithm_parameters.R")

test_that("Test lamp importing and image saving", {
    lamps <- load_lamps("./test/test_lights.csv", 0, 0, 0, ext=0)
    expect_equal(df_has_rows(lamps), FALSE)
    lamps <- load_lamps("./test/test_lights.csv", 0, 0, 0, ext=5)
    expect_equal(TRUE, TRUE)
})
