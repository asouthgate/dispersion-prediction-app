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
            sliderInput(inputId="radius", label="Radius:", min=1, max=1000, value=300)
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

    observeEvent(coord(), {
        print(coord())
    })

    observeEvent(input$radius, {
        print(input$radius)
    })

    observe({
        mapClick <- input$map_click
        if (is.null(mapClick)) return()
        print(paste("Latitude: ", mapClick$lat, "Longtitude: ", mapClick$lng))
        addMarkers(
            map(),
            mapClick$lng,
            mapClick$lat
        )
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
        paste(mapClick$lng, mapClick$lat, sep=",")
    })

}

shinyApp(ui=ui, server=server)
