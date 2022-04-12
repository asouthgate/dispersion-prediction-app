library(testthat)
library(R6)
library(glue)
library(rpostgis)

source("circuitscape_app/algorithm_parameters.R")
source("circuitscape_app/db.R")

test_that("Vector databases are available", {

    config <- configr::read.config("~/.bats.cfg")
    database_name <- config$database$name
    database_password <- config$database$password
    database_user <- config$database$user
    database_port <- config$database$port

    algorithm_parameters <- AlgorithmParameters$new(
        Roost$new(397093.199, 87698.191, 3000),
        RoadResistance$new(buffer=200, resmax=10, xmax=5),
        RiverResistance$new(buffer=200, resmax=10, xmax=5),
        LandscapeResistance$new(resmax=10, xmax=5),
        LinearResistance$new(buffer=200, resmax=10, xmax=5, rankmax=10),
        LampResistance$new(resmax=10, xmax=5, ext=100)
    )

    ext <- create_extent(algorithm_parameters)

    suppressWarnings({
        x <- read_db_vector("watercourselink", ext, database_host, database_name, database_port, database_user, database_password)
        y <- read_db_vector("roadlink", ext, database_host, database_name, database_port, database_user, database_password)
        z <- read_db_vector("building", ext, database_host, database_name, database_port, database_user, database_password)

        expect_gt(length(x), 0)
        expect_gt(length(y), 0)
        expect_gt(length(z), 0)
    })
})
#> Vector database SIT passed 🥳

test_that("Raster databases are available", {

    config <- configr::read.config("~/.bats.cfg")
    database_name <- config$database$name
    database_password <- config$database$password
    database_user <- config$database$user
    database_port <- config$database$port

    algorithm_parameters <- AlgorithmParameters$new(
        Roost$new(397093.199, 87698.191, 3000),
        RoadResistance$new(buffer=200, resmax=10, xmax=5),
        RiverResistance$new(buffer=200, resmax=10, xmax=5),
        LandscapeResistance$new(resmax=10, xmax=5),
        LinearResistance$new(buffer=200, resmax=10, xmax=5, rankmax=10),
        LampResistance$new(resmax=10, xmax=5, ext=100)
    )

    ext <- create_extent(algorithm_parameters)

    suppressWarnings({
        dtm <- read_db_raster("dtm", ext, database_host, database_name, database_port, database_user, database_password)
        dsm <- read_db_raster("dsm", ext, database_host, database_name, database_port, database_user, database_password)
        lcm <- read_db_raster("lcm", ext, database_host, database_name, database_port, database_user, database_password)

        expect_gt(length(dtm), 0)
        expect_gt(length(dsm), 0)
        expect_gt(length(lcm), 0)
    })
})
#> Raster database SIT passed 🥳