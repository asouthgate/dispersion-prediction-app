library(testthat)
library(mockery)
library(R6)
library(raster)

source("circuitscape_app/rasterfunc.R")
source("circuitscape_app/algorithm_parameters.R")


test_that("Test create ground rast functions", {

    groundrast <- create_ground_rast(0, 0, 100, 1)
    expect_equal(groundrast@ncols, 200)
    expect_equal(groundrast@nrows, 200)

})

test_that("Test distance raster functions", {

    coords <- data.frame(
        c(0, -50, 100, 0),
        c(0, -50, 60, 60)
    )

    sp <- SpatialPoints(coords)

    baserast <- raster(ncol=36, nrow=36)
    drast <- cal_distance_raster(sp, baserast)

    expect_equal(3587281.3, drast@data@values[[1]])

})


test_that("Test road and river resistance map creation", {

    buffer <- 200
    resmax <- 10
    xmax <- 5

    roads <- data.frame(
        c(0, -50, 100, 0),
        c(0, -50, 60, 60)
    )
    groundrast <- create_ground_rast(0, 0, 100, 1)

    ror <- cal_road_resistance(roads, groundrast, buffer, resmax, xmax)

    expect_equal(c(3.084, 3.117, 3.151, 3.185, 3.219, 3.254, 3.289, 3.325, 3.360, 3.397),
                ror@data@values[1:10])

    rir <- cal_river_resistance(roads, groundrast, buffer, resmax, xmax)

    expect_equal(c(1.045, 1.043, 1.042, 1.040, 1.038, 1.036, 1.035, 1.033, 1.032, 1.030),
                rir@data@values[1:10])
})

test_that("Test distance rasters", {

    x1 <- data.frame(
    c(0, -50, 100, 0),
    c(0, -50, 60, 60)
    )

    x2 <- data.frame(
    c(0, -20, 100, 0),
    c(0, -25, 60, 60)
    )

    x3 <- data.frame(
    c(0, -50, 10, 0),
    c(0, -50, 60, 60)
    )
    groundrast <- create_ground_rast(0, 0, 100, 1)
    d1 <- cal_distance_raster(x1, groundrast)
    d2 <- cal_distance_raster(x2, groundrast)
    d3 <- cal_distance_raster(x3, groundrast)

    distance_rasters <- matrix(c(d1, d2, d3, 1, 2, 4), nrow=3, ncol=2)

    dr <- distance2resistance(200, 10, 2, 10, 5, distance_rasters, "negative")

    expect_equal(1.148, dr@data@values[1])
})

test_that("Test circles creation", {

    groundrast <- create_ground_rast(0, 0, 100, 1)
    circles <- create_circles(groundrast, 0, 0, 100)

    expect_equal(c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
        circles@data@values[90:100])
})

test_that("Test calc surfaces", {

    groundrast <- create_ground_rast(0, 0, 100, 1)


    coords <- data.frame(
        c(0, -50, 100, 0),
        c(0, -50, 60, 60)
    )

    buildings <- raster::rasterize(SpatialPoints(coords), groundrast)


    buffer <- 200
    resmax <- 10
    xmax <- 5

    roads <- data.frame(
        c(0, -50, 100, 0),
        c(0, -50, 60, 60)
    )


    r1 <- cal_road_resistance(roads, groundrast, buffer, resmax, xmax)
    r2 <- cal_river_resistance(roads, groundrast, buffer, resmax, xmax)


    r_dtm <- raster::resample(r1, groundrast)

    r_dsm <- raster::resample(r2, groundrast)

    surfaces <- calc_surfs(r_dtm, r_dsm, buildings)

    expect_equal(-9.876, min(surfaces[[2]]@data@values))
    expect_equal(-0.992, max(surfaces[[1]]@data@values))

})