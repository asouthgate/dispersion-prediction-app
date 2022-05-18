library(testthat)
library(mockery)
library(R6)
library(raster)
library(ggplot2)
library(gridExtra)
library(grid)

source("circuitscape_app/rasterfunc.R")
source("circuitscape_app/resistance.R")
source("circuitscape_app/algorithm_parameters.R")

raster2df <- function(r) {
    val <- getValues(r)
    xy <- as.data.frame(xyFromCell(r,1:ncell(r)))
    xy <- cbind(xy,val)
    return(xy)
}

plot_setup <- function(name, distrast, soft_surf, linr) {

    linr <- raster2df(linr)
    soft <- raster2df(soft_surf)
    g1 <- ggplot() + geom_raster(data=linr, aes(x=x, y=y, fill=val)) + scale_fill_gradient(low="black", high="yellow", na.value="transparent")
    g2 <- ggplot() + geom_raster(data=soft, aes(x=x, y=y, fill=val)) + scale_fill_gradient(low="white", high="black", na.value="transparent")
    plist <- list(g1, g2)
    for (i in 1:nrow(distrast)) {
        rast <- distrast[[i, 1]]
        distd <- raster2df(rast)
        g3 <- ggplot() + geom_raster(data=distd, aes(x=x, y=y, fill=val)) + scale_fill_gradient(low="white", high="black", na.value="transparent")
        plist <- append(plist, list(g3))
    }
    gridg <- grid.arrange(grobs=plist, ncol=2)
    ggsave(name, gridg)

}

# TODO: find how to do common setup with testthat
test_that("Test linear resistance", {

    x <- 0
    y <- 0
    radius <- 100

    groundrast <- create_ground_rast(x, y, radius, 1)

    buffer <- 500
    resmax <- 22000
    xmax <- 3
    rankmax <- 4

    soft_surf <- groundrast

    soft_surf[1:1000] <- 0
    soft_surf[1000:2000] <- 1
    soft_surf[2000:3000] <- 2
    soft_surf[3000:4000] <- 3
    soft_surf[4000:5000] <- 4
    soft_surf[5000:6000] <- 5
    soft_surf[6000:7000] <- 8

    distance_rasters <- prep_lidar_rasters(soft_surf)

    linr <- get_linear_resistance(soft_surf, buffer, rankmax, resmax, xmax)

    plot_setup("test/tmp/test_linear_resistance.png", distance_rasters, soft_surf, linr)

    expect_equal(TRUE, TRUE)

})
