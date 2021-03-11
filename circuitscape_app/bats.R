#!/usr/bin/env Rscript

library(sf)
library(R6)
library(shiny)
library(raster)
library(leaflet)
library(rpostgis)
library(JuliaCall)

library(glue)

options(warn=-1)
options(shiny.port=8100)

source("algorithm_parameters.R")
source("generate.R")
source("ui.R")
source("server.R")

shinyApp(ui=ui, server=server)
