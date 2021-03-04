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

    coord <- reactive({
        x = as.numeric(input$easting)
        y = as.numeric(input$northing)
        c = convertPoint(x, y, 27700, 4326)
        return(c)
    })

    map <- reactive({
        leaflet() %>%
            addTiles() %>%
            setView(lng=coord()[1], lat=coord()[2], zoom=8)
    })

    output$coordinate <- renderText({
        paste(coord()[1], coord()[2], sep=",")
    })

    output$map <- renderLeaflet(map())

    observeEvent(coord(), {
        print(coord())
    })

}

shinyApp(ui=ui, server=server)
