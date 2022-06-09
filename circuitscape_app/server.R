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
library(shinybusy)
library(shinycssloaders)
library(stringr)
library(uuid)

source("R/algorithm_parameters.R")
source("R/pipeline.R")
source("R/transform.R")
source("circuitscape_app/drawing.R")
source("circuitscape_app/map_image_viewer.R")

if (!interactive()) sink(stderr(), type = "output")



#' Transform drawings to correct coordinates for existing pipeline
get_extra_geom <- function(drawings) {

    logger::log_debug("Trying to get extra geoms now")
    spdata <- drawings$get_spatial_data()
    data <- spdata$xy
    logger::log_debug("Got buildings:")
    extra_buildings <- data$building
    logger::log_debug("And extra buildings are:")
    extra_buildings_t <- NULL

    if (length(extra_buildings) > 0) {
        logger::log_debug("Attempting to get extra buildings")
        terra::crs(extra_buildings) <- sp::CRS("+init=epsg:4326")
        extra_buildings_t <- sp::spTransform(extra_buildings, "+init=epsg:27700")
    }

    extra_roads <- data$road
    extra_roads_t <- NULL

    # TODO: DRY
    if (length(extra_roads) > 0) {
        logger::log_debug("Attempting to get extra roads")
        logger::log_debug(extra_roads)
        terra::crs(extra_roads) <- sp::CRS("+init=epsg:4326")
        logger::log_debug(extra_roads)
        extra_roads_t <- sp::spTransform(extra_roads, "+init=epsg:27700")
        logger::log_debug(extra_roads_t)
    }

    extra_rivers <- data$river
    extra_rivers_t <- NULL

    if (length(extra_rivers) > 0) {
        logger::log_debug("Attempting to get extra rivers")
        logger::log_debug(extra_rivers)
        terra::crs(extra_rivers) <- sp::CRS("+init=epsg:4326")
        logger::log_debug(extra_rivers)
        extra_rivers_t <- sp::spTransform(extra_rivers, "+init=epsg:27700")
        logger::log_debug(extra_rivers_t)
    }

    logger::log_debug("getting extra lights")
    extra_lights <- data$lights
    if (length(extra_lights) > 0) {
        eldf <- data.frame(x=extra_lights$x, y=extra_lights$y, z=extra_lights$z)
        converted_pts <- vector_convert_points(eldf, 4326, 27700)
        extra_lights_t <- converted_pts
        extra_lights_t$z <- unlist(spdata$z$lights)
    } else {
        extra_lights_t <- data.frame(x=c(), y=c(), z=c())
    }

    logger::log_debug("Returning extras")
    return(list(extra_buildings=extra_buildings_t, extra_roads=extra_roads_t, 
                extra_rivers=extra_rivers_t, extra_lights=extra_lights_t,
                zvals=spdata$z
            ))
}

# Create an ST_Point object from x (longitude) and y (latitude) coordinates.
create_st_point <- function(x, y) {
    sf::st_point(c(as.numeric(x), as.numeric(y)))
}

vector_convert_points <- function(df, old, new) {
    logger::log_debug("Converting points...")
    coordsdf <- data.frame(newx=df$x, newy=df$y)
    old <- CRS(paste0("+init=epsg:", old))
    new <- CRS(paste0("+init=epsg:", new))
    spdf <- SpatialPointsDataFrame(data=df, coords=coordsdf, proj4string=old)
    spdf2 <- as.data.frame(spTransform(spdf, new))
    ret <- df
    ret$x <- spdf2$newx
    ret$y <- spdf2$newy
    logger::log_debug("Converted points")
    return(ret)
}

# # Convert coordinates from one EPSG coordinate system to another
convert_point <- function(x, y, source_crs, destination_crs) {
    source_point <- create_st_point(x, y)
    sfc <- sf::st_sfc(source_point, crs = source_crs)
    destination_point <- sf::st_transform(sfc, destination_crs)
    sf::st_coordinates(destination_point)
}

# Format coordinates to 3 decimal places
format_coordinate <- function(n) {
    if (is.null(n)) return("")
    format(n, digits = 3, nsmall = 3)
}

#Load the raster created by the Circuitscape algorithm and place it on the map
add_circuitscape_raster <- function(working_dir) {
    r <- raster::raster(paste0(working_dir, "/circuitscape/logCurrent.tif"))
    terra::crs(r) <- sp::CRS("+init=epsg:27700")
    leaflet::addRasterImage(leaflet::leafletProxy("map"), r, colors="Spectral", opacity=1)
}

server <- function(input, output, session) {

    # Disable some buttons at the beginning
    disable("generate")

    # Get the x coordinate of a reactive st_point
    x <- function(point) { point()[1] }

    # # Get the y coordinate of a reactive st_point
    y <- function(point) { point()[2] }

    # Set up the Leaflet map as a reactive variable
    map <- reactive({
        leaflet() %>%
            addTiles() %>%
            setView(lng=-2.045, lat=50.69, zoom=13)
    })
    output$map <- renderLeaflet(map())

    # Get the coordinates of the clicked map point in EPSG:4326 (WSG84)
    clicked4326 <- reactive({
        mapClick <- input$map_click
        if (is.null(mapClick)) return()
        create_st_point(mapClick$lng, mapClick$lat)
    })

    # Convert the coordinates of the clicked map point to EPSG:27700 (BNG)
    clicked27700 <- reactive({
        req(clicked4326())
        convert_point(x(clicked4326), y(clicked4326), 4326, 27700)
    })

    # Populate the roost coordinate text boxes from the map-click location
    output$easting <- renderText(format_coordinate(x(clicked27700)))
    output$northing <- renderText(format_coordinate(y(clicked27700)))
    output$longitude <- renderText(format_coordinate(x(clicked4326)))
    output$latitude <- renderText(format_coordinate(y(clicked4326)))

    delta <- 0.01;

    # values used to remember where the roost was when last clicked
    last_clicked_roost <- reactiveVal(c(0, 0))

    # a collection of drawings for the map
    drawings <- DrawingCollection$new(input, session, leafletProxy("map"))

    # Add/update map marker and circle at the clicked map point
    observeEvent(input$map_click, {
        logger::log_debug("Clicked on the map.")
        proxy <- leafletProxy("map")
        mapClick <- input$map_click
        # if (input$showRadius) {
        if (TRUE) {
            # If not currently selected a drawing
            # TODO: replace with a getter
            if (!is.null(drawings$selected_i)) {
                drawings$add_point_complete(proxy, mapClick$lng, mapClick$lat, input$map_zoom)
            }
            else {
                last_clicked_roost(c(mapClick$lng, lat=mapClick$lat))
                addMarkers(proxy, lng=last_clicked_roost()[1], lat=last_clicked_roost()[2], layerId="roost")
                addCircles(proxy, lng=last_clicked_roost()[1], lat=last_clicked_roost()[2], weight=1, radius=as.numeric(input$radius), layerId="roost")
            }
        }
    }, ignoreInit=TRUE)

    # Hide the radius circle when the checkbox is unchecked
    observeEvent(input$showRadius, {
        if (!input$showRadius) clearShapes(leafletProxy("map"))
    })

    observeEvent(input$streetLightsFile, {
        proxy <- leafletProxy("map")
        sldf <- vroom::vroom(input$streetLightsFile$datapath, delim=",")
    })

    # Upload street lights CSV file
    streetLightsData <- reactive({
        req(input$streetLightsFile)
        csv <- vroom::vroom(input$streetLightsFile$datapath, delim=",")
    })

    #Enable the raster download button when the file to download has been prepared
    enable_flags <- reactiveValues(resistance_complete=FALSE)
    observe({
        if (enable_flags$resistance_complete) {
            enable("generate_curr")
            enable("download")
        } else {
            disable("generate_curr")
            disable("download")
        }
        
    })

    uuid <- str_replace_all(UUIDgenerate(), "-", "_")
    workingDir = paste0("/tmp/circuitscape/", uuid)
    # a class for adding some rasters to the map
    miv <-  MapImageViewer$new(leafletProxy("map"))

    observeEvent(input$generate_res, {
        logger::log_info("Server: generate clicked")
        enable_flags$resistance_complete <- FALSE

        dir.create(workingDir, recursive = TRUE)
        dir.create(paste0(workingDir, "/circuitscape"))
        prepare_circuitscape_ini_file(workingDir)
        logger::log_debug(paste("workingDir is:", workingDir))

        roost <- c(x(clicked27700), y(clicked27700))
        radius <- input$radius
        logger::log_debug(paste("roost is:", roost[1], roost[2], radius))

        xy <- convert_point(last_clicked_roost()[1], last_clicked_roost()[2], 4326, 27700)
        # Collect the algorithm parameters from the user interface components
        algorithmParameters <- AlgorithmParameters$new(
            Roost$new(xy[1], xy[2], radius),
            RoadResistance$new(buffer=input$road_buffer, resmax=input$road_resmax, xmax=input$road_xmax),
            RiverResistance$new(buffer=input$river_buffer, resmax=input$river_resmax, xmax=input$river_xmax),
            LandscapeResistance$new(rankmax=input$landscape_rankmax, resmax=input$landscape_resmax, xmax=input$landscape_xmax),
            LinearResistance$new(buffer=input$linear_buffer, resmax=input$linear_resmax, rankmax=input$linear_rankmax, xmax=input$linear_xmax),
            LampResistance$new(resmax=input$lamp_resmax, xmax=input$lamp_xmax, ext=input$lamp_ext),
            resolution=input$resolution
        )

        logger::log_debug(paste("Running pipeline."))
        logger::log_debug(paste("Roost x, y, r: ", xy[1], xy[2], radius)) 
        logger::log_debug(paste("Road buffer, resmax, xmax ", input$road_buffer, input$road_resmax, input$road_xmax)) 
        logger::log_debug(paste("River buffer, resmax, xmax ", input$river_buffer, input$river_resmax, input$river_xmax)) 
        logger::log_debug(paste("Landscape resmax, xmax ", input$landscape_resmax, input$landscape_xmax)) 
        logger::log_debug(paste("Linear buffer, resmax, rankmax, xmax ", input$linear_buffer, input$linear_resmax, input$linear_rankmax, input$linear_xmax)) 
        logger::log_debug(paste("Lamp resmax, xmax, ext", input$lamp_resmax, input$lamp_xmax, input$lamp_ext))
        logger::log_debug(paste("Resolution", input$resolution))

        # reset the map image viewer, if there are previous images on
        miv$reset()

        tryCatch(
            {

                extra_geoms <- get_extra_geom(drawings)
                logger::log_debug("Got extra drawn inputs")

                logger::log_debug("Loading lamps")
                lamps <- data.frame(x=c(), y=c(), z=c())
                if (!is.null(input$streetLightsFile)) {
                    lamps <- load_lamps(input$streetLightsFile$datapath, algorithmParameters$roost$x, algorithmParameters$roost$y, algorithmParameters$roost$radius)
                }

                n_circles = input$n_circles

                input_data_fname =paste0(workingDir, "/input_data.Rdata")
                logger::log_info(paste("Saving initial data to ", input_data_fname))
                save(workingDir, n_circles, algorithmParameters, extra_geoms, lamps, file=input_data_fname)
                show_modal_spinner(text="Calculating resistance. This may take a few minutes...")

                submit_resistance_pipeline(input_data_fname) 

                load(paste0(workingDir, "/base_inputs.Rdata"))
                load(paste0(workingDir, "/resistance_maps.Rdata"))

                update_modal_spinner(text="Updating the map..")
                miv$add_initial_data(input, session, leafletProxy("map"), last_clicked_roost()[1], last_clicked_roost()[2], radius, base_inputs, resistance_maps)
                logger::log_info("Created map image viewer.")

                # Enable the download button
                enable_flags$resistance_complete <- TRUE
                remove_modal_spinner()

            },
            error=function(err) {
                warning(paste('Failed to generate raster :(', err$message))
                remove_modal_spinner()
                showNotification(paste('Failed to generate raster :(', err$message), duration=5, type="error")
            }
        )
    })

    observeEvent(input$generate_curr, {
        show_modal_spinner(text="Running circuitscape. This may take a few minutes...")
        logger::log_info("Pressed the current generation button...")
        tryCatch({
                logger::log_info("Calling circuitscape...")
                submit_circuitscape(workingDir)
                l_map <- raster(paste0(workingDir, "/circuitscape/log_current.tif"))
                print(miv)
                miv$add_current(session,l_map)
            },
            error=function(err) {
                warning(paste('Failed to generate raster :(', err$message))
                showNotification(paste('Failed to generate raster :(', err$message), duration=5, type="error")
            }
        )
        remove_modal_spinner()
    })

    output$download <- downloadHandler(
        filename <- function() {
            "rasters.zip"
        },
        content <- function(file) {
            logger::log_info("Download reuqired. Zipping files...")
            lcurr = paste0(workingDir, "/circuitscape/log_current.tif")
            lres = paste0(workingDir, "/circuitscape/log_resistance.tif")
            zip(file, c(lcurr, lres), extras = '-j')
        }
    )
}
