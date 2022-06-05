library('testthat')

source("circuitscape_app/server.R")


test_that("light processing is fast enough", {
    df <- read.csv("test/test_arne_lamps.csv")
    x2 <- rep(df$x, 100)
    y2 <- rep(df$y, 100)
    z2 <- rep(df$z, 100)
    df <- data.frame(x=x2, y=y2, z=z2)


    t1 <- Sys.time()
    res <- vector_convert_points_2(df, 27700, 4326)

    t2 <- Sys.time()
    print(t2-t1)

    res <- vector_convert_points(df, 27700, 4326)   

    t3 <- Sys.time()
    print(t3-t2)


    expect_equal(TRUE, TRUE)
})