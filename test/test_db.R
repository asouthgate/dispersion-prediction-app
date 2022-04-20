library(testthat)
library(R6)
library(glue)
library(rpostgis)
library(sp)
library(mockr)
library(DBI)

source("circuitscape_app/db.R")

mockgetgeom <- function(con, query) {
    message("No geometries found....")
    stop("failed randomly :(")
}

do_nothing <- function(driver, host, dbname, port, user, password) {}

do_nothing2 <- function(con) {}

test_that("DB exceptions work as expected", {


    local({
        suppressWarnings({
            local_mock(get_geom = mockgetgeom)
            local_mock(connect_to_db = do_nothing)
            local_mock(disconnect_db = do_nothing2)
            read_db_vector(1, 2, 3, 4, 5, 6, 7)
        })
        # should not crash
        expect_equal(TRUE, TRUE)
    })
})