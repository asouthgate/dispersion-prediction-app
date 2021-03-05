#!/usr/bin/env Rscript

library(sf)
library(raster)
library(glue)

options(shiny.port=8100)

generate <- function(roost, radius) {
    print("GENERATE")
    print(glue("roost=({roost[1]}, {roost[2]}); radius={radius}m"))
}

generate(
    roost=c(274257,66207),
    radius=300
)
