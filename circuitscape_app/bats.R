#!/usr/bin/env Rscript

library(sf)
library(shiny)
library(leaflet)

options(shiny.port=8100)

ui <- fluidPage(
    
    titlePanel("Bats"),

    sidebarLayout(
        sidebarPanel(
            h3("Roost Coordinates"),
            fluidRow(
                column(6, strong(p("Easting"))),
                column(6, strong(p("Northing")))
            ),
            fluidRow(
                column(6, verbatimTextOutput(outputId="easting")),
                column(6, verbatimTextOutput(outputId="northing"))
            ),
            fluidRow(
                column(6, strong(p("Longitude"))),
                column(6, strong(p("Latitude")))
            ),
            fluidRow(
                column(6, verbatimTextOutput(outputId="longitude")),
                column(6, verbatimTextOutput(outputId="latitude"))
            ),
            
            h3("Distance from Roost"),
            sliderInput(inputId="radius", label="Radius in meters:", min=100, max=1000, value=300),

            h3("Street Lighting"),
            fileInput("file", NULL, buttonLabel = "Upload CSV", accept=c(".csv"),  multiple=TRUE),
            tableOutput("files"),
            numericInput("n", "Rows", value=5, min=1, step=1),
            tableOutput("head")
        ),
        
        mainPanel(
            fillPage(leafletOutput("map"))
        )
    )
    
)

server <- function(input, output) {

    create_st_point <- function(x, y) { st_point(c(as.numeric(x), as.numeric(y))) }

    convertPoint <- function(x, y, sourceCRS, destinationCRS) {
        sourcePoint = create_st_point(x, y)
        sfc = st_sfc(sourcePoint, crs=sourceCRS)
        destinationPoint = st_transform(sfc, destinationCRS)
        coordinates = st_coordinates(destinationPoint)
        return(coordinates)
    }

    # Get the x coordinate of a reactive st_point
    x <- function(point) { point()[1] }

    # Get the y coordinate of a reactive st_point
    y <- function(point) { point()[2] }
    
    formatCoordinate <- function(n) { format(n, digits=3, nsmall=3) }

    map <- reactive({
        leaflet() %>%
            addTiles() %>%
            setView(lng=-3.964, lat=50.494, zoom=13)
    })

    output$map <- renderLeaflet(map())

    # Add/update map marker and circle at the clicked map point
    observe({
        mapClick <- input$map_click
        if (is.null(mapClick)) return()
        leafletProxy("map") %>%
            clearMarkers() %>%
            clearShapes() %>%
            addMarkers(lng=mapClick$lng, lat=mapClick$lat) %>%
            addCircles(lng=mapClick$lng, lat=mapClick$lat, weight = 1, radius=as.numeric(input$radius))
    })

    # Get the coordinate of the clicked map point in EPSG:27700 (BNG)
    clicked27700 <- reactive({
        mapClick <- input$map_click
        if (is.null(mapClick)) return()
        convertPoint(mapClick$lng, mapClick$lat, 4326, 27700)
    })

    # Get the coordinate of the clicked map point in EPSG:4326 (WSG84)
    clicked4326 <- reactive({
        mapClick <- input$map_click
        if (is.null(mapClick)) return()
        create_st_point(mapClick$lng, mapClick$lat)
    })

    # Roost Coordinates
    output$easting <- renderText(formatCoordinate(x(clicked27700)))
    output$northing <- renderText(formatCoordinate(y(clicked27700)))
    output$longitude <- renderText(formatCoordinate(x(clicked4326)))
    output$latitude <- renderText(formatCoordinate(y(clicked4326)))


    # File Upload
    data <- reactive({
        req(input$file)
        ext <- tools::file_ext(input$file$name)
        switch(
            ext,
            csv = vroom::vroom(input$file$datapath, delim = ","),
            validate("Invalid file; Please upload a .csv file")
        )
    })
    output$head <- renderTable({
        print(paste("datapath:", input$file$datapath))
        print(paste("type:", input$file$type))
        head(data(), input$n)
    })

}

shinyApp(ui=ui, server=server)
