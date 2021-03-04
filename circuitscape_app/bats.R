#!/usr/bin/env Rscript

library(sf)
library(shiny)
library(leaflet)

options(shiny.port=8100)

ui <- fluidPage(
    
    titlePanel("Bats"),

    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(6, textInput(inputId="easting", label="Easting:", value="274257")),
                column(6, textInput(inputId="northing", label="Northing:", value="66207"))
            ),
            fluidRow(
                column(12, h5("Coordinate")),
                column(12, verbatimTextOutput(outputId="coordinate"))
            ),
            fluidRow(
                column(12, h5("Clicked 27700")),
                column(12, verbatimTextOutput(outputId="clicked27700")),
            ),
            fluidRow(
                column(12, h5("Clicked 4326")),
                column(12, verbatimTextOutput(outputId="clicked4326")),
            ),
            sliderInput(inputId="radius", label="Radius:", min=1, max=1000, value=300),

            # File Upload
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

    convertPoint <- function(x, y, sourceCRS, destinationCRS) {
        x = as.numeric(x)
        y = as.numeric(y)
        sourcePoint = st_point(c(x, y))
        sfc = st_sfc(sourcePoint, crs=sourceCRS)
        destinationPoint = st_transform(sfc, destinationCRS)
        coordinates = st_coordinates(destinationPoint)
        return(coordinates)
    }

    coord <- reactive(convertPoint(input$easting, input$northing, 27700, 4326))

    map <- reactive({
        leaflet() %>%
            addTiles() %>%
            setView(lng=coord()[1], lat=coord()[2], zoom=12)
    })

    output$map <- renderLeaflet(map())

    output$coordinate <- renderText({
        paste(coord()[1], coord()[2], sep=",")
    })

    observeEvent(input$radius, {
        print(paste("Radius:", input$radius))
    })

    observe({
        mapClick <- input$map_click
        if (is.null(mapClick)) return()
        leafletProxy("map") %>%
            addMarkers(lng=mapClick$lng, lat=mapClick$lat)
    })

    output$clicked27700 <- renderText({
        mapClick <- input$map_click
        if (is.null(mapClick)) return()
        bngCoord <- convertPoint(mapClick$lng, mapClick$lat, 4326, 27700)
        x = bngCoord[1]
        y = bngCoord[2]
        paste(x, y, sep=",")
    })

    output$clicked4326 <- renderText({
        mapClick <- input$map_click
        if (is.null(mapClick)) return()
        paste(mapClick$lng, mapClick$lat, sep=",")
    })


    # File Upload

    output$files <- renderTable(input$upload)

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
        head(data(), input$n)
    })

}

shinyApp(ui=ui, server=server)
