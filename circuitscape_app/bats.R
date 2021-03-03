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
                column(
                    6,
                    textInput(
                        inputId="easting",
                        label="Easting:",
                        value="274257"
                    )
                ),
                column(
                    6,
                    textInput(
                        inputId="northing",
                        label="Northing:",
                        value="66207"
                    )
                )
            ),
            fluidRow(
                column(
                    12,
                    verbatimTextOutput(outputId="coordinate")
                )
            ),
            sliderInput(
                inputId="radius",
                label="Radius:",
                min=1,
                max=1000,
                value=300
            )
        ),
        mainPanel(
            fillPage(leafletOutput("map"))
        )
    )
    
)

server <- function(input, output) {

    convertPoint <- function(x, y, sourceCRS, destinationCRS) {
        sourcePoint = st_point(c(x, y))
        sfc = st_sfc(sourcePoint, crs=sourceCRS)
        destinationPoint = st_transform(sfc, destinationCRS)
        coordinates = st_coordinates(destinationPoint)
        return(coordinates)
    }

    output$coordinate <- renderText({
        x = as.numeric(input$easting)
        y = as.numeric(input$northing)
        c = convertPoint(x, y, 27700, 4326)
        x = c[1]
        y = c[2]
        paste(x, y, sep=",")
    })

    points <- eventReactive(input$recalc, {
        cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)
    
    output$map <- renderLeaflet({
        m <- leaflet() %>%
            addTiles() %>%
            setView(lng = -71.0589, lat = 42.3601, zoom=12) %>%
            addMarkers(data=points())
    })

}

shinyApp(ui=ui, server=server)
