#' Test that the cutom db code produces the same result as the library

library(testthat)
library(raster)

source("R/db.R")

args = commandArgs(trailingOnly=TRUE)

table <- "dsm"
db_host <- args[1]
db_name <- args[2]
db_port <- args[3]
db_user <- args[4]
db_pass <- args[5]

xmin <- 391848.990343289
xmax <- 393548.990343289
ymin <- 86054.6034798444
ymax <- 88054.6034798444


ext <- raster::extent(xmin, xmax, ymin, ymax)

t1 <- Sys.time()

r50 <- read_db_raster_custom2(table, ext, db_host, db_name, db_port, db_user, db_pass, resolution=50)
r25 <- read_db_raster_custom2(table, ext, db_host, db_name, db_port, db_user, db_pass, resolution=25)
r10 <- read_db_raster_custom2(table, ext, db_host, db_name, db_port, db_user, db_pass, resolution=10)
print(r50@extent)
print(r25@extent)
print(r10@extent)
r <- read_db_raster(table, ext, db_host, db_name, db_port, db_user, db_pass)


print(r@extent)
rc <- read_db_raster_custom2(table, ext, db_host, db_name, db_port, db_user, db_pass)
print(rc@extent)

jpeg(file="test/tmp/r50.jpeg")
plot(r50, col=grey(1:100/100))
dev.off()

jpeg(file="test/tmp/r25.jpeg")
plot(r25, col=grey(1:100/100))
dev.off()

jpeg(file="test/tmp/r10.jpeg")
plot(r10, col=grey(1:100/100))
dev.off()

jpeg(file="test/tmp/r.jpeg")
plot(r, col=grey(1:100/100))
dev.off()

jpeg(file="test/tmp/rc.jpeg")
plot(rc, col=grey(1:100/100))
dev.off()

print("done")




t2 <- Sys.time()

print(t2-t1)

r2 <- read_db_raster_custom2(table, ext, db_host, db_name, db_port, db_user, db_pass)

t3 <- Sys.time()

print(t3-t2)

r2 <- read_db_raster(table, ext, db_host, db_name, db_port, db_user, db_pass)

t4 <- Sys.time()

print(t4-t3)

expect_equal(values(r2), values(r3))

