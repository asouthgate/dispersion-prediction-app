#!/usr/bin/env Rscript

#
# This is the main file of the Bats Shiny app
#

#
# Load the required libraries
#
library(glue)
library(JuliaCall)
library(leaflet)
library(R6)
library(raster)
library(rpostgis)
library(sf)
library(shiny)
library(shinyBS)
library(shinyjs)
library(stringr)

#
# Set development options
#
options(warn=-1)
options(shiny.port=8100)

#
# Load the R source files
#
source("algorithm_parameters.R")
source("generate.R")
source("ui.R")
source("server.R")

#
# Start the Shiny app
#
shinyApp(ui=ui, server=server)
