#!/usr/bin/env Rscript

library(sf)
library(shiny)
library(raster)
library(leaflet)
library(rpostgis)
library(JuliaCall)

library(glue)

options(shiny.port=8100)

source("generate.R")
source("ui.R")
source("server.R")

shinyApp(ui=ui, server=server)
