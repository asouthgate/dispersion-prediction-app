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
library(uuid)

source("circuitscape_app/algorithm_parameters.R")
source("circuitscape_app/generate.R")
source("circuitscape_app/transform.R")

if (!interactive()) sink(stderr(), type = "output")

# The Circuitscape Julia function is parameterised by a .ini file
# that contains the paths of files required to perform the Circuitscape
# algorithm. These working files (including the .ini fie) are stored in a
# different randomly named folder for each use of the app. The file paths
# in the .ini must be customised to use the random working directory. We
# start with a template (cs.ini.template) and replace each occurence of
# WORKINGDIR with the working directory.
prepare_circuitscape_ini_file <- function(working_dir) {
    # Inject the working dir into the file ini template file
    template_filename <- "./cs.ini.template"
    template <- readChar(template_filename, file.info(template_filename)$size)
    output <- stringr::str_replace_all(template, "WORKINGDIR", working_dir)
    # Save the injected template in the working dir
    output_filename <- paste0(working_dir, "/cs.ini")
    output_file <- file(output_filename)
    writeLines(output, output_file)
    close(output_file)
}

# Create an ST_Point object from x (longitude) and y (latitude) coordinates.
create_st_point <- function(x, y) {
    sf::st_point(c(as.numeric(x), as.numeric(y)))
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

# dp <- DrawnPolygon$new()

#
# Define the server part of the Shiny application.
#
server <- function(input, output) {


    # Get the x coordinate of a reactive st_point
    x <- function(point) { point()[1] }

    # # Get the y coordinate of a reactive st_point
    y <- function(point) { point()[2] }

    test_global <- "TEST GLOBAL"

    dp <- DrawnPolygon$new()

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

    last_clicked_roost <- c(0, 0)

    # Add/update map marker and circle at the clicked map point
    observeEvent(input$map_click, {
        print("hmm... clicked")
        proxy <- leafletProxy("map")
        mapClick <- input$map_click
        if (is.null(mapClick)) return()

        if (input$showRadius) {
            proxy %>% clearMarkers() %>% clearShapes()
            if (input$draw_mode) {
                dp$add_point_complete(proxy, mapClick$lng, mapClick$lat, input$map_zoom)
            }
            else {
                last_clicked_roost <<- c(mapClick$lng, lat=mapClick$lat)
            }
            addMarkers(proxy, lng=last_clicked_roost[1], lat=last_clicked_roost[2])
            addCircles(proxy, lng=last_clicked_roost[1], lat=last_clicked_roost[2], weight=1, radius=as.numeric(input$radius))
        }
    })

    # Hide the radius circle when the checkbox is unchecked
    observeEvent(input$showRadius, {
        if (!input$showRadius) clearShapes(leafletProxy("map"))
    })

    observeEvent(input$clear_drawing, {
        proxy <- leafletProxy("map")
        dp$clear()
        clearShapes(proxy)
        # REPETITION: FIX
        addMarkers(proxy, lng=last_clicked_roost[1], lat=last_clicked_roost[2]) 
        addCircles(proxy, lng=last_clicked_roost[1], lat=last_clicked_roost[2], weight=1, radius=as.numeric(input$radius))
    })

    # Upload street lights CSV file
    streetLightsData <- reactive({
        req(input$streetLightsFile)
        csv <- vroom::vroom(input$streetLightsFile$datapath, delim=",")
    })

    # # Provide a preview of the first 5 lines of the uploaded lights CSV file
    # numberOfRowsToPreview <- 5
    # output$head <- renderTable({
    #     req(input$streetLightsFile)
    #     head(streetLightsData(), numberOfRowsToPreview)
    # })

    #Enable the raster download button when the file to download has been prepared
    downloadReady <- reactiveValues(ok=FALSE)
    observe({
        if (downloadReady$ok == TRUE) {
            enable("download")
        } else {
            disable("download")
        }
    })

    uuid <- str_replace_all(UUIDgenerate(), "-", "_")
    workingDir = paste0("/tmp/circuitscape/", uuid)

    observeEvent(input$generate, {
        # TODO: Disable the generate button until the street lights CSV file has been uploaded

        # Disable the download button
        downloadReady$ok <- FALSE

        # Generate the working directory for the current user of the app
        # workingDir <- "__working_dir__"
        # uuid <- str_replace_all(UUIDgenerate(), "-", "_")
        # workingDir = paste0("/tmp/circuitscape/", uuid)
        dir.create(workingDir, recursive = TRUE)
        dir.create(paste0(workingDir, "/circuitscape"))

        prepare_circuitscape_ini_file(workingDir)
        print(paste("workingDir:", workingDir))

        roost <- c(x(clicked27700), y(clicked27700))
        radius <- input$radius
        print(roost)

        # There are 17 steps that are monitored by the progress bar. Completing one step adds 100
        # to the progress score. We use 100 rather than 1 to enable multipart steps to increment
        # the progress bar after each subpart. For example, a step with 4 subparts would add 25
        # after completing each subpart.
        progressMax <- 17 * 100
        progress <- Progress$new(max=progressMax)
        on.exit(progress$close())

        # Collect the algorithm parameters from the user interface components
        algorithmParameters <- AlgorithmParameters$new(
            Roost$new(x(clicked27700), y(clicked27700), radius),
            RoadResistance$new(buffer=input$road_buffer, resmax=input$road_resmax, xmax=input$road_xmax),
            RiverResistance$new(buffer=input$river_buffer, resmax=input$river_resmax, xmax=input$river_xmax),
            LandscapeResistance$new(resmax=input$landscape_resmax, xmax=input$landscape_xmax),
            LinearResistance$new(buffer=input$linear_buffer, resmax=input$linear_resmax, rankmax=input$linear_rankmax, xmax=input$linear_xmax),
            LampResistance$new(resmax=input$lamp_resmax, xmax=input$lamp_xmax, ext=input$lamp_ext)
        )

        # Make sure the street lights CSV file has been uploaded
        req(input$streetLightsFile)

        # Set the message displayed by the progress bar
        progress$set(message="Generating resistance raster")

        # Start the algorithm to generate the bar dispersion raster
        tryCatch( {
                generate(
                    algorithmParameters=algorithmParameters,
                    workingDir=workingDir,
                    lightsFilename=input$streetLightsFile$datapath,
                    shinyProgress=progress,
                    progressMax=progressMax,
                    verbose=TRUE,
                    saveImages=TRUE
                )

                # Add the bat dispersion raster to the map
                add_circuitscape_raster(workingDir)

                # Enable the download button
                downloadReady$ok <- TRUE

            },
            error=function(err) {
                warning(paste('Failed to generate raster :(', err$message))
                showNotification(paste('Failed to generate raster :(', err$message), duration=5, type="error")
            }
        )
    })

    output$download <- downloadHandler(
        filename <- function() {
            print("get the filename")
            "logCurrent.tif"
        },
        content <- function(file) {
            print("get the content")
            rasterFilename = paste0(workingDir, "/circuitscape/logCurrent.tif")
            r <- raster(rasterFilename)
            # r <- raster("circuitscape/logCurrent.tif")
            crs(r) <- CRS("+init=epsg:27700")
            writeRaster(r, file, NAflag=-9999, overwrite=TRUE)
        }
    )
}
