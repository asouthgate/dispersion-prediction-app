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
library(ipc)

library(promises)
library(future)
# plan(multisession)
plan(multicore)

source("R/algorithm_parameters.R")
source("R/pipeline.R")
source("R/transform.R")
source("circuitscape_app/drawing_collection.R")
source("circuitscape_app/map_image_viewer.R")
source("R/rasterfunc.R")

if (!interactive()) sink(stderr(), type = "output")

marker_icon <- makeIcon(
  iconUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.8.0-beta.0/images/marker-icon.png",
  shadowUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.8.0-beta.0/images/marker-shadow.png",
  iconAnchorX = 12,
  iconAnchorY = 41
)

#' Transform drawings to correct coordinates for existing pipeline
get_extra_geom_from_drawings <- function(spdata) {

    logger::log_debug("Trying to get extra geoms now")
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

# Convert coordinates from one EPSG coordinate system to another
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

handle_resistance_completion <- function() {

}

async_run_pipeline <- function(session, input, progress, enable_flags, algorithm_parameters, workingDir, drawings, 
                                lamps, miv, currlat, currlon, radius, uuid) {

    # on.exit(progress$close())

    print(drawings)
    spdata <- drawings$get_spatial_data()
    extra_geoms <- get_extra_geom_from_drawings(spdata)

    print(extra_geoms)

    print("entering future...")

    showModal(modalDialog(
        title = "",
        "",
        easyClose = FALSE,
        footer = NULL
    ))

    # reset the map image viewer, if there are previous images on
    miv$reset()

    logger::log_info(paste("A user clicked the generate button with:"))
    logger::log_info(paste0("uuid, lat, lon, radius, resolution, n_buildings_drawn,",
                            "n_rivers_drawn, n_roads_drawn, n_lights_drawn, n_lights_imported")
                    )

    print(spdata)
    logger::log_info(paste(uuid, currlat, currlon, radius, algorithm_parameters$resolution,
        length(spdata$z$building), length(spdata$z$river),
        length(spdata$z$road), length(spdata$z$lights), length(lamps),
        sep=","
    ))


    future({

        print("Extra geoms again?:")
        print(extra_geoms)

        progress$set(message = "Preparing a few things...", value = 1)
        
        # Get extra geoms that have been drawn
        input_data_fname <- paste0(workingDir, "/input_data.Rdata")

        logger::log_info("Creating extent")
        ext <- create_extent(algorithm_parameters$roost$x, algorithm_parameters$roost$y, algorithm_parameters$roost$radius)
        algorithm_parameters$extent <- ext

        progress$set(message = "Generating ground raster...", value = 2)

        logger::log_info("Generating ground raster...")
        groundrast <- create_ground_rast(algorithm_parameters$roost$x, 
                                        algorithm_parameters$roost$y, 
                                        algorithm_parameters$roost$radius, 
                                        algorithm_parameters$resolution)

        logger::log_info("Attempting to fetch vector inputs")
        progress$set(message = "Fetching vector data...", value = 3)
        vector_inp <- fetch_vector_inputs(algorithm_parameters, workingDir)

        logger::log_info("Attempting to fetch faster inputs")
        progress$set(message = "Fetching LIDAR raster data...", value = 4)
        raster_inp <- fetch_raster_inputs(algorithm_parameters, groundrast, workingDir)

        logger::log_info("Getting extra height rasters for extra buildings...")
        # This is because, for the db buildings, height is obtained from lidar data, nothing exists for drawings
        progress$set(message = "Combining data from drawings...", value = 5)
        extra_height <- get_extra_height_rasters(groundrast, extra_geoms$extra_buildings, extra_geoms$zvals$building)

        logger::log_info("Adding the extra height from drawings")
        raster_inp$r_dsm <- raster_inp$r_dsm + extra_height

        db_input_fname <- paste0(workingDir, "/db_inputs.Rdata")
        algorithm_parameters_fname <- paste0(workingDir, "/algorithm_parameters.Rdata")

        # save(workingDir, raster_inp, file=db_input_fname)
        # save(workingDir, algorithm_parameters, groundrast, vector_inp, raster_inp, extra_geoms, lamps, file=input_data_fname)
        # save(workingDir, algorithm_parameters, file=algorithm_parameters_fname)

        logger::log_info("Combining inputs")
        progress$set(message = "Combining extra inputs...", value = 6)
        # submit_preprocess_pipeline(input_data_fname)
        base_inputs <- postprocess_inputs(algorithm_parameters, groundrast, vector_inp, raster_inp, workingDir, lamps, extra_geoms)

        # vector_inp$river <- base_inputs$river
        # vector_inp$road <- base_inputs$road

        logger::log_info("Submitting resistance pipeline")
        base_inputs_fname <- paste0(workingDir, "/base_inputs.Rdata")
        save(workingDir, algorithm_parameters, base_inputs, file=base_inputs_fname)


        progress$set(message = "Submitting to the resistance pipeline...", value = 7)
        submit_resistance_pipeline(base_inputs_fname)

        # load(paste0(workingDir, "/base_inputs.Rdata"))
        load(paste0(workingDir, "/resistance_maps.Rdata"))
        # TODO: bad; make base_inputs into a class that exposes debug rasters

        dsmnna <- raster_inp$dsm
        vals <- values(dsmnna)
        values(dsmnna)[is.na(vals)] <- 0

        resistance_maps$log_dsm <- log(dsmnna+1)
        resistance_maps$dsm <- raster_inp$dsm

        logger::log_info("Precomputing images for map")
        progress$set(message = "Processing images...", value = 8)
        # images <- miv$precompute_images(currlon, currlat, radius, base_inputs, raster_inp, vector_inp, resistance_maps)
        logger::log_info("Added miv initial data")

        list(resistance_maps=resistance_maps, disk=base_inputs$disk, raster_failed=raster_inp$raster_failed)
    }) %...>% (function(li) {

        removeModal()
        resistance_maps <- li$resistance_maps
        raster_failed <- li$raster_failed
        disk <- li$disk

        logger::log_info("Handling promise...")
        # load(paste0(workingDir, "/base_inputs.Rdata"))
        print(paste("failflag", raster_failed))
        if (raster_failed) {
            # add a warning flag to the panel
            print("inserting ui element")
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
        # load(paste0(workingDir, "/resistance_maps.Rdata"))

        # remove_modal_spinner()
        # show_modal_spinner(text="Updating the map..")
        progress$set(message = "Adding images to map...", value = 9)
        # miv$load_precomputed_images(currlon, currlat, radius, images)
        # miv$add_ui(input, session)

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
        logger::log_info("Created map image viewer.")

        # Enable the download button
        enable_flags$resistance_complete <- TRUE
        progress$close()
    })
}

server <- function(input, output, session) {

    uuid <- str_replace_all(UUIDgenerate(), "-", "_")
    logger::log_info(paste("New session created with UUID", uuid))

    # Disable some buttons at the beginning
    disable("generate")

    # Get the x coordinate of a reactive st_point
    x <- function(point) { point()[1] }

    # # Get the y coordinate of a reactive st_point
    y <- function(point) { point()[2] }

    # Set up the Leaflet map as a reactive variable
    map <- reactive({
        leaflet(options = leafletOptions()) %>%
        # htmlwidgets::onRender("function(el, x) {
        #     L.control.zoom({ position: 'bottomleft' }).addTo(this)
        # }") %>%
        addTiles() %>%
        setView(lng=-3.18108916282654, lat=51.4866309794335, zoom=13)
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

    observeEvent(input$latitude_input, {
        # output$latitude <- renderText(lati)
        lati <- input$latitude_input
        # setView(map=leafletProxy("map"), lng=last_clicked_roost()[1], lat=lati, zoom=13)
        last_clicked_roost(c(last_clicked_roost()[1], lati))
        # TODO: DRY!
        proxy <- leafletProxy("map")
        addMarkers(proxy, lng=last_clicked_roost()[1], lat=last_clicked_roost()[2], icon = marker_icon,layerId="roost")
        addCircles(proxy, lng=last_clicked_roost()[1], lat=last_clicked_roost()[2], weight=1, radius=as.numeric(input$radius), layerId="roost")
    })

    # TODO: DRY!
    observeEvent(input$longitude_input, {
        loni <- input$longitude_input
        # setView(map=leafletProxy("map"), lng=loni, lat=last_clicked_roost()[2], zoom=13)
        last_clicked_roost(c(loni, last_clicked_roost()[2]))
        # output$longitude <- renderText(loni)
        proxy <- leafletProxy("map")
        addMarkers(proxy, lng=last_clicked_roost()[1], lat=last_clicked_roost()[2], icon = marker_icon,layerId="roost")
        addCircles(proxy, lng=last_clicked_roost()[1], lat=last_clicked_roost()[2], weight=1, radius=as.numeric(input$radius), layerId="roost")
        # updated_latlon_via_click <- FALSE
    })

    # Populate the roost coordinate text boxes from the map-click location
    output$easting <- renderText(format_coordinate(x(clicked27700)))
    output$northing <- renderText(format_coordinate(y(clicked27700)))
    # output$longitude <- renderText(format_coordinate(x(clicked4326)))
    # output$latitude <- renderText(format_coordinate(y(clicked4326)))

    # values used to remember where the roost was when last clicked
    last_clicked_roost <- reactiveVal(c(-2.104, 50.684))

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
                last_clicked_roost(c(mapClick$lng, mapClick$lat))
                addMarkers(proxy, lng=last_clicked_roost()[1], lat=last_clicked_roost()[2], icon = marker_icon,layerId="roost")
                addCircles(proxy, lng=last_clicked_roost()[1], lat=last_clicked_roost()[2], weight=1, radius=as.numeric(input$radius), layerId="roost")
                updateNumericInput(session, "latitude_input", value=last_clicked_roost()[2])
                updateNumericInput(session, "longitude_input", value=last_clicked_roost()[1])
            }
        }
    }, ignoreInit=TRUE)

    observeEvent(input$radius, {
        proxy <- leafletProxy("map")
        addMarkers(proxy, lng=last_clicked_roost()[1], lat=last_clicked_roost()[2], icon = marker_icon,layerId="roost")
        addCircles(proxy, lng=last_clicked_roost()[1], lat=last_clicked_roost()[2], weight=1, radius=as.numeric(input$radius), layerId="roost")
    })

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

    observeEvent(input$radius, {
        # limit max resolution based on radius
        max_n_pixel <- 1000000
        max_row_pixel <- sqrt(max_n_pixel)
        d <- input$radius * 2
        min_resolution <- round(max(1, d / max_row_pixel))
        updateSliderInput(session, "resolution", value = max(input$resolution, min_resolution),
            min = min_resolution)
    })

    workingDir = paste0("/tmp/circuitscape/", uuid)
    # a class for adding some rasters to the map
    miv <-  MapImageViewer$new(leafletProxy("map"))

    observeEvent(input$generate_res, {

        drawings$unselect_all(session)

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

        # Load lamps
        lamps <- data.frame(x=c(), y=c(), z=c())
        if (!is.null(input$streetLightsFile)) {
            lamps <- load_lamps(input$streetLightsFile$datapath, 
                                algorithm_parameters$roost$x, 
                                algorithm_parameters$roost$y, 
                                algorithm_parameters$roost$radius)
        }

        # Set up directories 
        logger::log_info(paste("Creating workingDir:", workingDir))
        dir.create(workingDir, recursive = TRUE)
        dir.create(paste0(workingDir, "/circuitscape"))

        tryCatch(
            {
                # show_modal_spinner(text="Calculating resistance. This may take a few minutes...", color="#3a3a3d")
                print(drawings)
                progress <- AsyncProgress$new(session, min=1, max=10, message="Preparing...", value = 0)
                async_run_pipeline(session, input, progress, enable_flags, algorithm_parameters, workingDir, 
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
        prepare_circuitscape_ini_file(workingDir)
        tryCatch({
                logger::log_info("Calling circuitscape...")
                progress <- AsyncProgress$new(session, min=1, max=10, message="Preparing...", value = 0)
                future({
                    progress$set(message = "Calculating current map with Circuitscape...", value = 5)
                    submit_circuitscape(workingDir)
                }) %...>% (function(images) {
                    progress$set(message = "Adding images...", value = 9)
                    l_map <- raster(paste0(workingDir, "/circuitscape/log_current.tif"))
                    miv$add_current(session,l_map)
                    progress$close()
                })
            },
            error=function(err) {
                warning(paste('Failed to generate raster :(', err$message))
                showNotification(paste('Failed to generate raster :(', err$message), duration=5, type="error")
            }
        )
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
