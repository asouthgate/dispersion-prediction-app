library(testthat)
library(mockery)
library(R6)
library(raster)

source("circuitscape_app/rasterfunc.R")
source("circuitscape_app/generate.R")
source("circuitscape_app/algorithm_parameters.R")
source("test/old_rasterfunc.R")

# test_that("Calculating point irradiance produces same result as old implementation", {

#     # x <- 600
#     # y <- 600
#     # z <- 10

#     rast <- raster::raster(xmn=300, xmx=900, ymn=300, ymx=900, resolution=1)

#     # zero resistance raster, should have simple illumination
#     values(rast) <- 0
#     hard_surf <- rast
#     values(hard_surf) <- NA
#     soft_surf <- rast
#     values(soft_surf) <- NA
#     terrain <- rast

#     lampsdf <- data.frame(x=rnorm(mean=600, n=5, sd=100), y=rnorm(mean=600, n=5, sd=100), z=rnorm(mean=50, n=5, sd=5))
#     old_point_irradiance <- calc_point_irradiance_old(lampsdf, soft_surf, hard_surf, terrain)
#     expect_false(mean(values(old_point_irradiance)) == 0)

#     point_irradiance <- calc_point_irradiance(lampsdf, soft_surf, hard_surf, terrain)
#     expect_false(mean(values(point_irradiance)) == 0)

#     print(mean(values(point_irradiance)))
#     expect_equal(values(point_irradiance), values(old_point_irradiance))

# })

# test_that("Calculating point irradiance works properly with lamps around the boundary", {

#     x <- 305
#     y <- 300
#     z <- 10
#     rast <- raster::raster(xmn=300, xmx=900, ymn=300, ymx=900, resolution=1)

#     # zero resistance raster, should have simple illumination
#     values(rast) <- 0
#     hard_surf <- rast
#     values(hard_surf) <- NA
#     soft_surf <- rast
#     values(soft_surf) <- NA
#     terrain <- rast

#     lampsdf <- data.frame(x=c(x), y=c(y), z=c(z))
    
#     old_point_irradiance <- calc_point_irradiance_old(lampsdf, soft_surf, hard_surf, terrain)
#     print(old_point_irradiance)
#     expect_false(mean(values(old_point_irradiance)) == 0)

#     point_irradiance <- calc_point_irradiance(lampsdf, soft_surf, hard_surf, terrain)
#     expect_false(mean(values(point_irradiance)) == 0)

#     expect_equal(values(point_irradiance), values(old_point_irradiance))

# })




# test_that("Calculating lit area extraction functions work correctly", {

#     x <- 0
#     y <- 0
#     z <- 0
#     delta <- 50
#     rast <- raster::raster()
#     values(rast) <- 0
#     ri <- raster::rowFromY(rast, y)
#     cj <- raster::colFromX(rast, x)
#     rast[1:ri-3, ] <- 100
#     area <- cal_light_surface_indices(x, y, z, rast, delta)
#     expect_equal(105.5, mean(unlist(area)))

#     hard_surf <- rast
#     soft_surf <- rast
#     values(soft_surf) <- 0
#     soft_surf[100:ri-3, ] <- 200
#     terrain <- rast
#     values(terrain) <- 0
#     terrain[500:ri-3, ] <- 300

#     hard_surf <- rast
#     values(hard_surf) <- (seq_len(length(soft_surf)) / 3)
#     soft_surf <- rast
#     values(soft_surf) <- (seq_len(length(soft_surf)) * 2)
#     terrain <- rast
#     values(terrain) <- (seq_len(length(soft_surf)) * 3.1)

#     tblocks <- get_blocks(hard_surf, soft_surf, terrain, area$ri_min, area$cj_min, area$ncols, area$nrows)
#     expect_equal(c(100, 100), dim(tblocks$soft_block))
#     expect_equal(29260, tblocks$soft_block[100])
#     expect_equal(c(100, 100), dim(tblocks$hard_block))
#     expect_equal(4876.666667, tblocks$hard_block[100], tolerance=1e-5)
#     expect_equal(c(100, 100), dim(tblocks$terrain_block))
#     expect_equal(tblocks$terrain_block[100], 45353)

#     pia <- cal_irradiance_arr(area$ri_lamp, area$cj_lamp, z, area$ncol, area$nrow, delta, tblocks$terrain_block, tblocks$hard_block, tblocks$soft_block)

#     expect_equal(mean(pia), 0.00008417237, tolerance=0.0000001)

#     lampsdf <- data.frame(x=c(x), y=c(y), z=c(z))
#     old_point_irradiance <- calc_point_irradiance_old(lampsdf, soft_surf, hard_surf, terrain)
#     point_irradiance <- calc_point_irradiance(lampsdf, soft_surf, hard_surf, terrain)

#     expect_equal(mean(values(point_irradiance)), mean(values(old_point_irradiance)))
# })


# TODO: expand tests

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

# TODO: find how to do common setup with testthat
test_that("Test calc surfaces, landscape, and linear resistance", {

    x <- 0
    y <- 0
    radius <- 100

    groundrast <- create_ground_rast(x, y, radius, 1)

    coords <- data.frame(
        c(0, -50, 100, 0),
        c(0, -50, 60, 60)
    )

    buildings <- raster::rasterize(SpatialPoints(coords), groundrast)


    buffer <- 200
    resmax <- 10
    xmax <- 5
    rankmax <- 5

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

    lcr <- get_landscape_resistance_lcm(r_dtm, buildings, surfaces)

    expect_equal(7.084, lcr@data@values[[1]])

    surfaces$soft_surf[1:1000] <- 0
    surfaces$soft_surf[1000:2000] <- 1
    surfaces$soft_surf[2000:3000] <- 2
    surfaces$soft_surf[3000:4000] <- 3
    surfaces$soft_surf[4000:5000] <- 4
    surfaces$soft_surf[5000:6000] <- 5
    surfaces$soft_surf[6000:7000] <- 8

    prep_lidar_rasters(surfaces$soft_surf)

    linr <- get_linear_resistance(surfaces$soft_surf, buffer, rankmax, resmax, xmax)

    expect_equal(2.1594306, mean(linr@data@values))

    lamps <- load_lamps("test/test_lights.csv", x, y, radius)

    lamp_ext <- 100

    # lampres <- cal_lamp_resistance(lamps, surfaces$soft_surf, surfaces$hard_surf, dtm,
    #                     lamp_ext, resmax, xmax)

})

test_that("Prep lidar tifs produces expected results", {
    groundrast <- create_ground_rast(0, 0, 100, 1)

    coords <- data.frame(
        c(1, -50, 100, 0),
        c(0, -50, 60, 60)
    )

    surf <- raster::rasterize(SpatialPoints(coords), groundrast)
    surf[is.na(surf)] <- 1
    surf[1:10] <- 1:10

    rast <- prep_lidar_rasters(surf)

    expect_equal(c(0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5),
        rast[[1, 1]]@data@values[10:20]
    )
    expect_equal(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        rast[[2, 1]]@data@values[20:30]
    )
    expect_equal(c(0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8),
        rast[[3, 1]]@data@values[10:20]
    )


})

test_that("Boundary function works right", {
    expect_equal(0.5, bound_val(0.5, 0, 1))
    expect_equal(-1, bound_val(-1, -10, 1))
    expect_equal(bound_val(-11, -10, 1), -10)
    expect_equal(bound_val(-11, -10, -3), -10)
})

