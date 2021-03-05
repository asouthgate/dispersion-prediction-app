#!/usr/bin/env Rscript

library(sf)
library(shiny)
library(leaflet)
library(raster)
library(glue)

options(shiny.port=8100)

source("ui.R")
source("server.R")

shinyApp(ui=ui, server=server)
