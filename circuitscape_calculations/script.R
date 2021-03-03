library(shiny)
library(plyr)
library(shinyMatrix)
library(ggplot2)
library(grid)
library(gridExtra)
library(rgdal)
library(raster)
library(rgeos)
library(tools) 
library(leaflet)
library(sf)
library(tidyverse)
library(rdrop2)
library(rsconnect)
library(pryr)
library(JuliaCall)

setwd("/Users/work/Programming/Bats/circuitscape_app")

# token <- drop_auth()
# drop_auth(rdstoken = "my-token.rds") #for dropbox

roost = c(274257,66207)
radius = 300
resolution = 1
xmax = roost[1]+radius
ymax = roost[2]+radius
xmin = roost[1]-radius
ymin = roost[2]-radius
ext <- extent(xmin,xmax,ymin,ymax)

ground_rast <- function(roost, radius, resolution) {
  #Generates a ground raster: NA everywhere except roost coordinates, used for resampling
  groundrast <- raster(xmn= roost[1] - radius,   # set minimum x coordinate
                       xmx = roost[1] + radius,    # set maximum x coordinate
                       ymn = roost[2] - radius,     # set minimum y coordinate
                       ymx = roost[2] + radius,     # set maximum y coordinate
                       res = c(resolution,resolution),
                       crs = NA) 
  roosts <- matrix(c(roost[1],roost[2]), nrow = 1, ncol = 2)
  groundrast <- rasterize(roosts,groundrast)
  writeRaster(groundrast, "gis-layers/ground.asc", overwrite=TRUE)
  groundrast
}

groundrast <- ground_rast(roost, radius, resolution)


output_dir = './gis-layers'

select_coords <- function(xmin,xmax,ymin,ymax) {
  gridref <- read.csv(file="grid_reference.csv")
  xmaxs <- round_any(xmax, 1e5, f = floor)
  xmins <- round_any(xmin, 1e5, f = floor)
  ymaxs <- round_any(ymax, 1e5, f = floor)
  ymins <- round_any(ymin, 1e5, f = floor)
  grid <- gridref[(gridref$Easting==xmaxs|gridref$Easting==xmins),]
  grid <- grid[grid$Northing==ymaxs|grid$Northing==ymins,]
  grid[[1]]
}

grid <- select_coords(xmin, xmax,ymin,ymax)
grid

read_roads <- function(grid, ext, ground) {
  if (length(grid[[1]])>1) {  #This is for if the extent goes across >1 grid squares
    road <- readOGR(dsn="./gis-layers/road",layer=paste0(grid[[1]][1],"_RoadLink"))
    road <- crop(road, ext)
    for (i in 2:length(grid[[1]])) {
      road2 <- readOGR(dsn="gis-layers/road",layer=paste0(grid[[1]][i],"_RoadLink"))
      road2 <- crop(road2,ext)
      roadfinal <- bind(road,road2)
    }
  } else {
    road <- readOGR(dsn="./gis-layers/road",layer=paste0(grid[[1]], "_RoadLink"))
    road <- crop(road, ext)
  }
  road
}

road = read_roads(grid,ext,ground)
plot(road)
