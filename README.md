# Introduction

This repository contains the code that implements the [Shiny](https://shiny.rstudio.com) app for the *Predicting bat dispersion through urban environments* project. Most of the calculations are performed by [R](https://www.r-project.org) to set up the inputs to the [Circuitscape](https://docs.circuitscape.org/Circuitscape.jl/latest/) calculation that is implemented in [Julia](https://julialang.org). The app queries a [PostGIS](https://postgis.net) database for the vector and raster data required to perform the calculations.

# Installation

## R Packages

Run the following commands in the R console to install the required packages:

```R
install.packages("glue")
install.packages("JuliaCall")
install.packages("leaflet")
install.packages("R6")
install.packages("raster")
install.packages("rpostgis")
install.packages("sf")
install.packages("shiny")
install.packages("shinyBS")
install.packages("shinyjs")
install.packages("stringr")
install.packages("uuid")
install.packages("vroom")
```

## Julia Package

Run the following commands in the Julia console to install the required Circuitscape package:

```julia
using Pkg
Pkg.add("Circuitscape")
```

# Environment Variables

The default values for the PostgreSQL database name and the PostgreSQL port are:

```bash
DATABASE_NAME="os"
DATABASE_PORT=5432
```

Configure any of these values by creating a `.env` file with different values, e.g.:

```bash
DATABASE_PORT=5555
DATABASE_NAME="my-bat-data"
```

# Data

The data is all open source. The shapefiles for buildings, rivers and roads are from Ordnance Survey:

* https://www.ordnancesurvey.co.uk/business-government/products/open-map-rivers
* https://www.ordnancesurvey.co.uk/business-government/products/open-map-roads
* https://www.ordnancesurvey.co.uk/business-government/products/open-map-local
 
The landcover is from the CEH landcover map:

* https://www.ceh.ac.uk/services/land-cover-map-2015
 
Lidar DTM/DSM is also used to figure out where hedgerows/forest is, which is available from the government: 

* https://data.gov.uk/dataset/fba12e80-519f-4be2-806f-41be9e26ab96/lidar-composite-dsm-2017-2m
* https://data.gov.uk/dataset/002d24f0-0056-4176-b55e-171ba7f0e0d5/lidar-composite-dtm-2017-2m


# Resources

Some useful articles:

* [How can Postgis and R be used as a GIS?](https://rstudio-pubs-static.s3.amazonaws.com/304489_1a4dff62928e4ffeb4267e15cff254ca.html)
