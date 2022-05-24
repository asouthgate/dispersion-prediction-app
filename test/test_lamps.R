library(testthat)
library(R6)
library(glue)
library(rpostgis)
library(sp)
library(mockr)
library(ggplot2)
library(gridExtra)
library(grid)

source("circuitscape_app/generate.R")
source("circuitscape_app/algorithm_parameters.R")
source("circuitscape_app/rasterfunc.R")
source("circuitscape_app/irradiance.R")

source("test/old_rasterfunc.R")

test_that("Test lamp importing and image saving without crashing", {
    lamps <- load_lamps("./test/test_lights.csv", 0, 0, 0, ext=0)
    expect_equal(nrow(lamps) > 0, FALSE)
    lamps <- load_lamps("./test/test_lights.csv", 0, 0, 0, ext=5)
    expect_equal(TRUE, TRUE)
})

test_that("We can compile the Rcpp code", {
    sourceCpp("circuitscape_app/irradiance.cpp")
    expect_equal(TRUE, TRUE)
})

raster2df <- function(r) {
    val <- getValues(r)
    xy <- as.data.frame(xyFromCell(r,1:ncell(r)))
    xy <- cbind(xy,val)
    return(xy)
}

plot_setup <- function(name, irradiance, lights, hard_surface, soft_surface, terrain) {

    irr <- raster2df(irradiance)
    hard <- raster2df(hard_surface)
    soft <- raster2df(soft_surface)
    g1 <- ggplot() + geom_raster(data=irr, aes(x=x, y=y, fill=val)) + scale_fill_gradient(low="black", high="yellow", na.value="transparent")
    g2 <- ggplot() + geom_raster(data=hard, aes(x=x, y=y, fill=val)) + scale_fill_gradient(low="white", high="black", na.value="transparent")
    g3 <- ggplot() + geom_raster(data=soft, aes(x=x, y=y, fill=val)) + scale_fill_gradient(low="white", high="gray50", na.value="transparent")
    g4 <- ggplot() + geom_point(data=lights, aes(x=x, y=y))
    gridg <- grid.arrange(g1, g2, g3, g4, ncol=2) 
    ggsave(name, gridg)   

}

plot_irradiance <- function(name, irradiance) {

    irr <- raster2df(irradiance)
    g1 <- ggplot() + geom_raster(data=irr, aes(x=x, y=y, fill=val)) + scale_fill_gradient(low="black", high="yellow", na.value="transparent")
    ggsave(name, g1)   

}

test_that("Calculating point irradiance produces same result as old implementation", {

    rast <- raster::raster(xmn=300, xmx=900, ymn=300, ymx=900, resolution=1)

    # zero resistance raster, should have simple illumination
    values(rast) <- 0
    hard_surf <- rast
    values(hard_surf) <- NA
    soft_surf <- rast
    values(soft_surf) <- NA
    terrain <- rast

    lampsdf <- data.frame(x=rnorm(mean=600, n=5, sd=100), y=rnorm(mean=600, n=5, sd=100), z=rnorm(mean=50, n=5, sd=5))

    old_point_irradiance <- calc_point_irradiance_old(lampsdf, soft_surf, hard_surf, terrain)
    point_irradiance <- wrap_cal_irradiance(lampsdf, soft_surf, hard_surf, terrain, sensor_ht=1.5)

    plot_irradiance("test/tmp/old_pir_1.png", old_point_irradiance)
    plot_irradiance("test/tmp/new_pir_1.png", point_irradiance)

    expect_false(mean(values(point_irradiance)) == 0)
    expect_false(mean(values(old_point_irradiance)) == 0)

    # a few pixels can be different
    mpir <- mean(values(point_irradiance))
    old_mpir <- mean(values(old_point_irradiance))
    tol <- mpir/100
    print(paste(mpir, old_mpir, tol))
    expect_equal(mpir, old_mpir, tolerance=tol)

})


test_that("Illumination works for a simple lighting scenarios", {

    r <- raster::raster(xmn=-50, xmx=50, ymn=-50, ymx=50, resolution=1)
    values(r) <- 0
    hard_surface <- r
    soft_surface <- r 
    terrain <- r

    lampdf <- data.frame(x=c(0), y=c(0), z=c(10))

    irr <- wrap_cal_irradiance(lampdf, soft_surface, hard_surface, terrain)
    plot_setup("test/tmp/lights_test_1.png", irr, lampdf, hard_surface, soft_surface, terrain)

    hard_surface <- r
    soft_surface <- r 
    terrain <- r
    hard_surface[20:30,] = 50
    irr <- wrap_cal_irradiance(lampdf, soft_surface, hard_surface, terrain)
    plot_setup("test/tmp/lights_test_2.png", irr, lampdf, hard_surface, soft_surface, terrain)

    hard_surface <- r
    soft_surface <- r 
    terrain <- r
    soft_surface[,46:47] = 50
    irr <- wrap_cal_irradiance(lampdf, soft_surface, hard_surface, terrain)
    plot_setup("test/tmp/lights_test_3.png", irr, lampdf, hard_surface, soft_surface, terrain)

    lampdf <- data.frame(x=c(0, 5), y=c(0, 4), z=c(10, 20))
    hard_surface <- r
    soft_surface <- r 
    terrain <- r
    irr <- wrap_cal_irradiance(lampdf, soft_surface, hard_surface, terrain)
    plot_setup("test/tmp/lights_test_4.png", irr, lampdf, hard_surface, soft_surface, terrain)

    lampdf <- data.frame(x=c(0, 49, 9), y=c(0, 4, -19), z=c(10, 10, 10))
    hard_surface <- r
    soft_surface <- r 
    terrain <- r
    irr <- wrap_cal_irradiance(lampdf, soft_surface, hard_surface, terrain)
    plot_setup("test/tmp/lights_test_4.png", irr, lampdf, hard_surface, soft_surface, terrain)

    expect_equal(TRUE, TRUE)
})