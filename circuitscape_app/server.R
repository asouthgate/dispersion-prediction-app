#' Main server code
#'

library(leaflet)
library(R6)
library(raster)
library(rgdal)
library(rpostgis)
library(sf)
library(shiny)
library(shinyBS)
library(shinyjs)
library(uuid)
library(ipc)

library(promises)
library(future)
plan(multicore)

source("R/algorithm_parameters.R")
source("R/pipeline.R")
source("R/transform.R")
source("circuitscape_app/drawing_collection.R")
source("circuitscape_app/map_image_viewer.R")
source("R/rasterfunc.R")

if (!interactive()) sink(stderr(), type = "output")

# Required due to a bug when deploying in certain environments
marker_icon <- makeIcon(
  iconUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.8.0-beta.0/images/marker-icon.png",
  shadowUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.8.0-beta.0/images/marker-shadow.png",
  iconAnchorX = 12,
  iconAnchorY = 41
)

#' Create an ST_Point object from x (longitude) and y (latitude) coordinates.
create_st_point <- function(x, y) {
    sf::st_point(c(as.numeric(x), as.numeric(y)))
}

#' Convert x, y coordinates from one EPSG coordinate system to another
convert_point <- function(x, y, source_crs, destination_crs) {
    source_point <- create_st_point(x, y)
    sfc <- sf::st_sfc(source_point, crs = source_crs)
    destination_point <- sf::st_transform(sfc, destination_crs)
    sf::st_coordinates(destination_point)
}

#' Format coordinates to 3 decimal places
format_coordinate <- function(n) {
    if (is.null(n)) return("")
    format(n, digits = 3, nsmall = 3)
}

#' Run the main pipeline asynchronously
#'
#' To prevent the app from freezing up, the pipeline is run async
#'
#' @param session
#' @param input
#' @param progress an AsyncProgress object, regular ShinyProgress will not work with futures
#' @param enable_flags flags used to enable certain UI components
#' @param algorithm_parameters parameters for the algorithm itself
#' @param working_dir working directory where outputs and tmp files are saved
#' @param drawings a DrawingCollection
#' @param lamps an additional csv of lamps
#' @param miv a MapImageViewer object for rendering the results
#' @param currlat the current latitude of the roost
#' @param currlon the current longitude of the roost
#' @param radius the roost radius
#' @param uuid the uuid for the user
async_run_pipeline <- function(session, input, progress, enable_flags, algorithm_parameters, working_dir, drawings,
                                lamps, miv, currlat, currlon, radius, uuid) {

    # Get spatial data frames from the collection of drawings
    spdfs <- drawings$get_spatial_dfs(crs = "27700")

    # Show empty modal to get the blanked out screen effect
    showModal(modalDialog(
        title = "",
        "",
        easyClose = FALSE,
        footer = NULL
    ))

    # Reset the map image viewer, if there are previous images on
    miv$reset(session)
    logger::log_info("Reset the miv in first pipeline")
    # miv <-  MapImageViewer$new(leafletProxy("map"))

    # Log the positions etc of the clicks

    logger::log_info(paste("A user clicked the generate button with:"))
    logger::log_info(paste0("uuid, lat, lon, radius, resolution, n_buildings_drawn,",
                            "n_rivers_drawn, n_roads_drawn, n_lights_drawn, n_lights_imported"))

    n_buildings <- 0
    n_roads <- 0
    n_rivers <- 0
    n_lights <- 0

    if (!is.null(spdfs$buildings)) n_buildings <- nrow(spdfs$buildings)
    if (!is.null(spdfs$roads)) n_roads <- nrow(spdfs$roads)
    if (!is.null(spdfs$rivers)) n_rivers <- nrow(spdfs$rivers)
    if (!is.null(spdfs$lights)) n_lights <- nrow(spdfs$lights)

    logger::log_info(paste(uuid, currlat, currlon, radius, algorithm_parameters$resolution,
        n_buildings, n_rivers, n_roads, n_lights,
        sep = ","
    ))

    future({

        # Firstly, do some preparation

        progress$set(message = "Preparing a few things...", value = 1)

        input_data_fname <- paste0(working_dir, "/input_data.Rdata")

        # Create an extent, essentially a bounding box
        logger::log_info("Creating extent")
        ext <- create_extent(algorithm_parameters$roost$x, algorithm_parameters$roost$y, algorithm_parameters$roost$radius)
        algorithm_parameters$extent <- ext

        progress$set(message = "Generating ground raster...", value = 2)

        # Generate the ground raster, that is the sink for circuitscape
        # Also used as a kind of template for the resistance map pipeline
        logger::log_info("Generating ground raster...")
        groundrast <- create_ground_rast(algorithm_parameters$roost$x,
                                        algorithm_parameters$roost$y,
                                        algorithm_parameters$roost$radius,
                                        algorithm_parameters$resolution)

        # Fetch vector data from the db
        logger::log_info("Attempting to fetch vector inputs")
        progress$set(message = "Fetching vector data...", value = 3)
        vector_inp <- fetch_vector_inputs(algorithm_parameters, working_dir)

        # Fetch raster data from the db
        logger::log_info("Attempting to fetch faster inputs")
        progress$set(message = "Fetching LIDAR raster data...", value = 4)
        raster_inp <- fetch_raster_inputs(algorithm_parameters, groundrast, working_dir)

        # Add extra height; for the db buildings, height is obtained from lidar data, nothing exists for drawings
        logger::log_info("Getting extra height rasters for extra buildings...")
        progress$set(message = "Combining data from drawings...", value = 5)
        if (!is.null(spdfs$buildings)) {
            extra_height <- get_extra_height_rasters(groundrast, SpatialPolygons(spdfs$buildings@polygons), spdfs$buildings$heights)
            logger::log_info("Adding the extra height from drawings")
            raster_inp$r_dsm <- raster_inp$r_dsm + extra_height
        }

        # Do some more data merging for the extra drawings
        logger::log_info("Combining inputs")
        progress$set(message = "Combining extra inputs...", value = 6)
        base_inputs <- postprocess_inputs(algorithm_parameters, groundrast, vector_inp, raster_inp, working_dir, lamps, spdfs)

        # Save a few data files for separate submission to the resistance pipeline queue
        logger::log_info("Submitting resistance pipeline")
        base_inputs_fname <- paste0(working_dir, "/base_inputs.Rdata")
        save(working_dir, algorithm_parameters, base_inputs, file = base_inputs_fname)

        progress$set(message = "Submitting to the resistance pipeline...", value = 7)
        submit_resistance_pipeline(base_inputs_fname)

        load(paste0(working_dir, "/resistance_maps.Rdata"))

        logger::log_info("Precomputing images for map")
        progress$set(message = "Processing images...", value = 8)
        logger::log_info("Added miv initial data")

        list(resistance_maps = resistance_maps, disk = base_inputs$disk, raster_failed = raster_inp$raster_failed)

    }) %...>% (function(li) {

        # Remove the modal background
        removeModal()

        resistance_maps <- li$resistance_maps
        raster_failed <- li$raster_failed
        disk <- li$disk

        logger::log_info("Handling promise...")

        # If the raster failed flag is present, we will add a warning
        if (raster_failed) {
            removeUI(selector="div:has(> #warning_div)", immediate = TRUE)
            insertUI(
                selector = "#download",
                where = "afterEnd",
                div(
                    id="warning_div",
                    br(),
                    code("Warning: some data is missing! Results may be inaccurate.")
                )
            )
        }

        progress$set(message = "Adding images to map...", value = 9)

        # Calculate the log total resistance
        logtotalres <- resistance_maps$total_res
        navals <- is.na(values(logtotalres))
        logtotalres[navals] <- 0
        logtotalres <- logtotalres + 1
        logtotalres <- log(logtotalres)

        rmaps_to_show <- list(
            "Total Resistance"=resistance_maps$total_res,
            "Log Total Resistance"=logtotalres,
            "Road Resistance"=resistance_maps$road_res,
            "River Resistance"=resistance_maps$river_res,
            "Landscape Resistance"=resistance_maps$landscape_res,
            "Linear Resistance"=resistance_maps$linear_res,
            "Lamp Resistance"=resistance_maps$lamp_res
        )

        miv$load_plain_rasters(input, session, currlon, currlat, radius, rmaps_to_show, disk)
        logger::log_info("Loaded rasters into MIV.")

        # Enable the download button
        enable_flags$resistance_complete <- TRUE
        progress$close()
    })
}

server <- function(input, output, session) {

    # Create uuid and set up temp folders for the session
    uuid <- str_replace_all(UUIDgenerate(), "-", "_")
    logger::log_info(paste("New session created with UUID", uuid))

    working_dir <- paste0("/tmp/circuitscape/", uuid)
    logger::log_info(paste("Creating working_dir:", working_dir))
    dir.create(working_dir, recursive = TRUE)
    dir.create(paste0(working_dir, "/circuitscape"))

    # Disable some buttons at the beginning
    disable("generate_curr")

    # Set up the Leaflet map as a reactive variable
    map <- reactive({
        leaflet(options = leafletOptions()) %>%
        addTiles() %>%
        setView(lng = -3.18108916282654, lat = 51.4866309794335, zoom = 13)
    })
    output$map <- renderLeaflet(map())

    # Get the coordinates of the clicked map point in EPSG:4326 (WSG84)
    clicked4326 <- reactive({
        map_click <- input$map_click
        if (is.null(map_click)) return()
        create_st_point(map_click$lng, map_click$lat)
    })

    # Convert the coordinates of the clicked map point to EPSG:27700 (BNG)
    clicked27700 <- reactive({
        req(clicked4326())
        convert_point(clicked4326()[1], clicked4326()[2], 4326, 27700)
    })

    # Values used to remember where the roost was when last clicked
    last_clicked_roost <- reactiveVal(c(-2.104, 50.684))

    # Observe the latitude input box
    observeEvent(input$latitude_input, {
        lati <- input$latitude_input
        last_clicked_roost(c(last_clicked_roost()[1], lati))
        # TODO: repetition
        proxy <- leafletProxy("map")
        addMarkers(proxy, lng = last_clicked_roost()[1], 
            lat = last_clicked_roost()[2], icon = marker_icon, layerId = "roost")
        addCircles(proxy, lng = last_clicked_roost()[1], 
            lat = last_clicked_roost()[2], weight = 1, radius = as.numeric(input$radius), layerId = "roost")
    })

    # TODO: repetition
    # Observe the longitude input box
    observeEvent(input$longitude_input, {
        loni <- input$longitude_input
        last_clicked_roost(c(loni, last_clicked_roost()[2]))
        proxy <- leafletProxy("map")
        addMarkers(proxy, lng = last_clicked_roost()[1],
                    lat = last_clicked_roost()[2], icon = marker_icon, layerId="roost")
        addCircles(proxy, lng = last_clicked_roost()[1],
                    lat = last_clicked_roost()[2], weight = 1, radius = as.numeric(input$radius), layerId = "roost")
        nepoint <- convert_point(last_clicked_roost()[1], last_clicked_roost()[2], 4326, 27700)
        output$northing <- renderText(nepoint[1])
        output$easting <- renderText(nepoint[2])
    })

    # Populate the roost coordinate text boxes from the map-click location
    output$easting <- renderText(format_coordinate(clicked27700()[1]))
    output$northing <- renderText(format_coordinate(clicked27700()[2]))


    # A collection of drawings for the map
    drawings <- DrawingCollection$new(input, session, leafletProxy("map"))

    # Main map click response code
    observeEvent(input$map_click, {
        logger::log_debug("Clicked on the map.")
        proxy <- leafletProxy("map")
        map_click <- input$map_click
        print(drawings$something_is_selected())
        if (drawings$something_is_selected()) {
            drawings$add_point_complete(map_click$lng, map_click$lat)
        } else {
            last_clicked_roost(c(map_click$lng, map_click$lat))
            addMarkers(proxy, lng = last_clicked_roost()[1], lat = last_clicked_roost()[2], 
                icon = marker_icon, layerId = "roost")
            addCircles(proxy, lng = last_clicked_roost()[1], lat = last_clicked_roost()[2],
                weight = 1, radius = as.numeric(input$radius), layerId = "roost")
            updateNumericInput(session, "latitude_input", value = last_clicked_roost()[2])
            updateNumericInput(session, "longitude_input", value = last_clicked_roost()[1])
        }
    }, ignoreInit = TRUE)

    # Observe if radius changes: redraw and recalculate permissible resolution
    observeEvent(input$radius, {

        proxy <- leafletProxy("map")
        addMarkers(proxy, lng = last_clicked_roost()[1],
            lat = last_clicked_roost()[2], icon = marker_icon, layerId = "roost")
        addCircles(proxy, lng = last_clicked_roost()[1],
            lat = last_clicked_roost()[2], weight = 1, radius = as.numeric(input$radius), layerId = "roost")
        
        max_n_pixel <- 1000000
        max_row_pixel <- sqrt(max_n_pixel)
        d <- input$radius * 2
        min_resolution <- round(max(1, d / max_row_pixel))
        updateSliderInput(session, "resolution", value = max(input$resolution, min_resolution),
        min = min_resolution)

    })

    # Hide the radius circle when the checkbox is unchecked
    observeEvent(input$showRadius, {
        if (!input$showRadius) clearShapes(leafletProxy("map"))
    })

    # Observe if drawing
    observeEvent(input$upload_file, {

        logger::log_info("Got file upload click")

        folder <- dirname(input$upload_file$datapath)

        unzip(input$upload_file$datapath, exdir = folder)

        tryCatch({drawings$read_buildings(folder)},
            error = function(e) { print(e); logger::log_error("No buildings")})
        tryCatch({drawings$read_roads(folder)}, 
            error = function(e) { print(e); logger::log_error("No roads")})
        tryCatch({drawings$read_rivers(folder)}, 
            error = function(e) { print(e); logger::log_error("No rivers")})
        tryCatch({drawings$read_lights(paste0(folder, "/lights.csv"))}, 
            error = function(e) { print(e); logger::log_error("No lights")})

    })

    # Upload street lights CSV file
    # streetLightsData <- reactive({
    #     print("Street lights, something is happening")
    #     req(input$streetLightsFile)
    #     print("Street lights, something is happening")
    #     # csv <- vroom::vroom(input$streetLightsFile$datapath, delim = ",")
    #     # Load lamps
    #     if (!is.null(input$streetLightsFile)) {
    #         lamps <- load_lamps(input$streetLightsFile$datapath, 
    #                             algorithm_parameters$roost$x, 
    #                             algorithm_parameters$roost$y, 
    #                             algorithm_parameters$roost$radius)
    #         print("lamps loaded")
    #         print(lamps)
    #     }
    #     lamps()
    # })

    lamps <- data.frame(x=c(), y=c(), z=c())

    observeEvent(input$streetLightsFile, {
        if (!is.null(input$streetLightsFile)) {
            drawings$read_lights_variable_heights(input$streetLightsFile$datapath)
        }
    })

    # Enable the raster download button when the file to download has been prepared
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

    # A class for adding some rasters to the map
    miv <-  MapImageViewer$new(leafletProxy("map"))

    observeEvent(input$generate_res, {

        drawings$unselect_all()

        # Disable completion; can't download while running
        enable_flags$resistance_complete <- FALSE

        # Get roost params
        roost <- last_clicked_roost()
        currlon <- roost[1]
        currlat <- roost[2]
        radius <- input$radius

        # Convert to 27700
        northeast <- convert_point(last_clicked_roost()[1], last_clicked_roost()[2], 4326, 27700)

        # Create algorithm parameters
        algorithm_parameters <- AlgorithmParameters$new(
            Roost$new(northeast[1], northeast[2], radius),
            RoadResistance$new(buffer=input$road_buffer, resmax=input$road_resmax, xmax=input$road_xmax),
            RiverResistance$new(buffer=input$river_buffer, resmax=input$river_resmax, xmax=input$river_xmax),
            LandscapeResistance$new(rankmax=input$landscape_rankmax, resmax=input$landscape_resmax, xmax=input$landscape_xmax),
            LinearResistance$new(buffer=input$linear_buffer, resmax=input$linear_resmax, rankmax=input$linear_rankmax, xmax=input$linear_xmax),
            LampResistance$new(resmax=input$lamp_resmax, xmax=input$lamp_xmax, ext=input$lamp_ext),
            resolution=input$resolution,
            n_circles=input$n_circles
        )

        # # Load lamps
        # lamps <- data.frame(x=c(), y=c(), z=c())
        # if (!is.null(input$streetLightsFile)) {
        #     lamps <- load_lamps(input$streetLightsFile$datapath, 
        #                         algorithm_parameters$roost$x, 
        #                         algorithm_parameters$roost$y, 
        #                         algorithm_parameters$roost$radius)
        #     print("lamps loaded")
        #     print(lamps)
        # }

        tryCatch(
            {
                # show_modal_spinner(text="Calculating resistance. This may take a few minutes...", color="#3a3a3d")
                print(drawings)
                print(session)
                progress <- AsyncProgress$new(session, min=1, max=10, message="Preparing...", value = 0)
                async_run_pipeline(session, input, progress, enable_flags, algorithm_parameters, working_dir, 
                                    drawings, lamps, miv, currlat, currlon, radius, uuid)

            },
            error=function(err) {
                warning(paste('Failed to generate raster :(', err$message))
                logger::log_debug("Failed to generate raster:")
                print(err$message)
                showNotification(paste('Failed to generate raster :(', err$message), duration=5, type="error")
            }
        )
    })

    observeEvent(input$generate_curr, {
        logger::log_info("Pressed the current generation button...")
        prepare_circuitscape_ini_file(working_dir)
        tryCatch({
                logger::log_info("Calling circuitscape...")
                progress <- AsyncProgress$new(session, min=1, max=10, message="Preparing...", value = 0)
                showModal(modalDialog(
                    title = "",
                    "",
                    easyClose = FALSE,
                    footer = NULL
                ))

                future({
                    progress$set(message = "Calculating current map with Circuitscape...", value = 5)
                    submit_circuitscape(working_dir)
                }) %...>% (function(images) {
                    progress$set(message = "Adding images...", value = 9)
                    l_map <- raster(paste0(working_dir, "/circuitscape/log_current.tif"))
                    print(l_map)
                    miv$add_current(session, l_map)
                    progress$close()
                    disable("generate_curr")
                    removeModal()
                })
            },
            error=function(err) {
                warning(paste('Failed to generate raster :(', err$message))
                showNotification(paste('Failed to generate raster :(', err$message), duration=5, type="error")
            }
        )
    })

    output$download_drawings <- downloadHandler(
        filename <- function() {
            "drawings.zip"
        },
        content <- function(file) {
            logger::log_info(paste("Download reuqired. Zipping files to: ", file))
            shp_dir = paste0(working_dir, "/shape_data")
            drawings$write(shp_dir)
            zip(file, shp_dir, extras = '-j')
        }
    )

    output$download <- downloadHandler(
        filename <- function() {
            "rasters.zip"
        },
        content <- function(file) {
            logger::log_info("Download reuqired. Zipping files...")
            lcurr = paste0(working_dir, "/circuitscape/log_current.tif")
            lres = paste0(working_dir, "/circuitscape/log_resistance.tif")
            zip(file, c(lcurr, lres), extras = '-j')
        }
    )
}
