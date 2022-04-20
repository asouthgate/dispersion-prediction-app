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

PIP1 <- "     xxxxxxxxxxxx
  xxxxx        xxxxxx
 xxx               xxxxxxxx
xxvxxxx     xx xxx        xxx                     ▲xxxxxxx
vv    xxxxx             xx xxx                   xvx xx  xxxxxxxx
      vvx              xx    xx   ▲         ▲   xx x  xxxx      xxxx
       vvvvv          xx     xxx xx        xx   x xx     xxxx     xx xxx
          ''''        x      x  xx xx    xx x xx   x        xxx      x  x
             '''      x        ((   xx  xxx   x    x          xxxx   xxxxxx
               ''      x       x     xx xx   ))    x              xxx    xxx
                 v  vv x                      x   xx              '' xx v vvv
                      ''            {O   O}      x       '''''''''''' v
                       ''   x   x     (oo)      x     ''''
                        ''    xxx           x   xxvvv ''
                         ''  xxxx          x xx
                          x xx xxx       xx x
                          vvxx  x x'   x vx
                          v  v  xxxxxxxxvv
                                 xxxx  v
                                vx
"
#
# Define the user interface part of the Shiny app
#
ui <- fluidPage(
    useShinyjs(),

    div(class="outer",
    
        tags$head(
            includeCSS("./style.css")
        ),
        
        # h1(id="big-heading", PIP1, class="ascii-art"),

        leafletOutput("map", width="100%", height="100%"),

        # absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        #     draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        #     width = 330, height = "auto",
        absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
            draggable = FALSE, top = "5%", left = "auto", right = "5%", bottom = "10%",
            # width = 330, height = "auto",

            # sidebarPanel(id = "scrollymcscrollface",
                h2(id="big-heading", PIP1, class="ascii-art"),

                tableOutput("head"),

                bsCollapse(id="collapseParameters", open="collapsePanel",
                    bsCollapsePanel("🞻  Street Lights", style="default",
                        fileInput("streetLightsFile", NULL, buttonLabel="Upload CSV", accept=c(".csv"),  multiple=TRUE)
                    ),
                    bsCollapsePanel("⚙  Resistance Parameters", style="default",
                        bsCollapsePanel(
                            "Road",
                            numericInput("road_buffer", "Buffer", value=200, min=1, max=100, step=1),
                            numericInput("road_resmax", "Resmax", value=10, min=1, max=100, step=1),
                            numericInput("road_xmax", "Xmax", value=5, min=1, max=100, step=1),
                            style="default"
                        ),
                        bsCollapsePanel(
                            "River",
                            numericInput("river_buffer", "Buffer", value=10, min=1, max=100, step=1),
                            numericInput("river_resmax", "Resmax", value=2000, min=1, max=100, step=1),
                            numericInput("river_xmax", "Xmax", value=4, min=1, max=100, step=1),
                            style="default"
                        ),
                        bsCollapsePanel(
                            "Landscape",
                            numericInput("landscape_resmax", "Resmax", value=100, min=1, max=100, step=1),
                            numericInput("landscape_xmax", "Xmax", value=5, min=1, max=100, step=1),
                            style="default"
                        ),
                        bsCollapsePanel(
                            "Linear",
                            numericInput("linear_buffer", "Buffer", value=10, min=1, max=100, step=1),
                            numericInput("linear_resmax", "Resmax", value=22000, min=1, max=100, step=1),
                            numericInput("linear_resmax", "Rankmax", value=4, min=1, max=100, step=1),
                            numericInput("linear_xmax", "Xmax", value=3, min=1, max=100, step=1),
                            style="default"
                        ),
                        bsCollapsePanel(
                            "Lamp",
                            numericInput("lamp_resmax", "Resmax", value=1e8, min=1, max=100, step=1),
                            numericInput("lamp_xmax", "Xmax", value=1, min=1, max=100, step=1),
                            numericInput("lamp_ext", "Ext", value=100, min=1, max=100, step=1),
                            style="default"
                        )
                    ),
                    bsCollapsePanel(
                        "◯  Roost",
                        numericInput("road_buffer", "Buffer", value=200, min=1, max=100, step=1),
                        sliderInput(inputId="radius2", label="Radius in meters:", min=100, max=1000, value=300),
                        checkboxInput(inputId="showRadius2", label="Show radius", value=TRUE),
                        style="default"
                    ),
                    bsCollapsePanel(
                        "◿  Drawing",
                        checkboxInput(inputId="draw_mode2", label="Draw mode", value=FALSE),
                        style="default"
                    )

                ),

                h4("Raster"),
                actionButton(inputId="generate", label="Generate Raster"),
                downloadButton(outputId="download", label="Download Raster")
        )
    )
        # ),
        
        # mainPanel(
        #     fillPage(
                
                # tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                # tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                #leafletOutput("map", width = "100%", height = "100%")
                # leafletOutput("map")
                # br(),
                # fluidRow(
                #     column(6,
                #         h4("Roost Coordinates"),
                #         fluidRow(
                #             column(6, strong(p("Easting"))),
                #             column(6, strong(p("Northing")))
                #         ),
                #         fluidRow(
                #             column(6, verbatimTextOutput(outputId="easting")),
                #             column(6, verbatimTextOutput(outputId="northing"))
                #         ),
                #         fluidRow(
                #             column(6, strong(p("Longitude"))),
                #             column(6, strong(p("Latitude")))
                #         ),
                #         fluidRow(
                #             column(6, verbatimTextOutput(outputId="longitude")),
                #             column(6, verbatimTextOutput(outputId="latitude"))
                #         )
                #     ),
                #     column(5, offset=1,
                #         h4("Distance from Roost"),
                #         sliderInput(inputId="radius", label="Radius in meters:", min=100, max=1000, value=300),
                #         checkboxInput(inputId="showRadius", label="Show radius", value=TRUE),
                #         checkboxInput(inputId="draw_mode", label="Draw mode", value=FALSE),
                #     )
                # )
            # )
        # )
    
)
