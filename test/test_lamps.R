library(testthat)
library(R6)
library(glue)
library(rpostgis)
library(sp)
library(mockr)
library(ggplot2)

source("circuitscape_app/generate.R")
source("circuitscape_app/algorithm_parameters.R")
source("circuitscape_app/rasterfunc.R")
source("circuitscape_app/irradiance.R")


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

plot_setup <- function(name, irr, lights, hard_surface, soft_surface, terrain) {
    ggplot() + geom_raster(data=raster2df(irr), aes(x=x, y=y, fill=val)) +
        # geom_raster(data=raster2df(hard_surface), aes(x=x, y=y, fill=val)) +
        # scale_color_gradient(low="white", high="gray50", na.value="transparent") +
        geom_point(data=lights, aes(x=x, y=y))
    # ggplot() + geom_raster(data=raster2df(irr), aes(x=x, y=y, fill=val))
    ggsave(name)    
}

test_that("Illumination works for a simple light with no obstacles", {
    r <- raster::raster(xmn=-5, xmx=5, ymn=-5, ymx=5, resolution=1)
    print(r)
    values(r) <- 0
    hard_surface <- r
    soft_surface <- r 
    terrain <- r

    lampdf <- data.frame(x=c(0), y=c(0), z=c(10))
    t1 <- Sys.time()
    irr <- calc_point_irradiance(lampdf, hard_surface, soft_surface, terrain)
    t2 <- Sys.time()
    irrcpp <- wrap_cal_irradiance(lampdf, hard_surface, soft_surface, terrain)
    t3 <- Sys.time()
    print(paste(t2-t1, t3-t2))
    plot_setup("test/tmp/lights_test_1.png", irr, lampdf, hard_surface, soft_surface, terrain)

    hard_surface[,1:3] = 50
    irr <- calc_point_irradiance(lampdf, hard_surface, soft_surface, terrain)
    plot_setup("test/tmp/lights_test_2.png", irr, lampdf, hard_surface, soft_surface, terrain)

    plot_setup("test/tmp/lights_test_3_cpp.png", irrcpp, lampdf, hard_surface, soft_surface, terrain)



    expect_equal(TRUE, TRUE)
})
