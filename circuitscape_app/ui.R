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
library(bslib)

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
PIP2 <- "                                                             xxxxxxxxxxxx
                                                        xxxxxx        xxxxx
                                                  xxxxxxxx               xxx
                   xxxxxxx                     xxx        xxx xx     xxxxvxx
            xxxxxxxx  xx xvx                   xxx xx             xxxxx   vv
         xxxx      xxxx  x xx   ▲         ▲   xx    xx              xvv
     xxx xx     xxxx     xx x   xx        xx xxx     xx          vvvvv
    x  x      xxx        x   xx x xx    xx xx  x      x        ''''
  xxxxxx   xxxx          x    x   xxx  xx   ))        x      '''
 xxx    xxx              x    ((   xx xx     x       x      ''
vvv v xx ''              xx   x                      x vv  v
      v ''''''''''''       x      {O   O}            ''
                   ''''     x      (oo)     x   x   ''
                     '' vvvxx   x           xxx    ''
                              xx x          xxxx  ''
                                x xx       xxx xx x
                                  xv x   'x x  xxvv
                                   vvxxxxxxxx  v  v
                                     v  xxxx
                                           xv

"

ui <- fluidPage(
    useShinyjs(),


    div(class="outer",
        tags$head(
            includeCSS("./circuitscape_app/style.css")
        ),
        leafletOutput("map", width="100%", height="100%"),

        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
            draggable = FALSE, top = "0%", left = "auto", right = "0%",
            bottom = "5%", style="justify-content:center",
            # width="auto",
            # width = 330, height = "auto",

            # h4(id="big-heading", PIP2, class="ascii-art"),
            div(style="display: inline-block;vertical-align:top;min-width:20vw"),

            tags$div(class = "header", style="width:100%"),

            bsCollapse(id="collapseParameters", open="collapsePanel",
                bsCollapsePanel("🞻  Street Lights", style="default",
                    fileInput("streetLightsFile", NULL, buttonLabel="Upload CSV", accept=c(".csv"),  multiple=TRUE)
                ),
                bsCollapsePanel("⚙  Resistance Parameters", style="default",
                    bsCollapsePanel(
                        "Road",
                        numericInput("road_buffer", "Buffer", value=200, min=1, max=1000, step=1),
                        numericInput("road_resmax", "Resmax", value=10, min=1, max=10000, step=1),
                        numericInput("road_xmax", "Xmax", value=5, min=1, max=10, step=1),
                        style="default"
                    ),
                    bsCollapsePanel(
                        "River",
                        numericInput("river_buffer", "Buffer", value=10, min=1, max=100, step=1),
                        numericInput("river_resmax", "Resmax", value=2000, min=1, max=10000, step=1),
                        numericInput("river_xmax", "Xmax", value=4, min=1, max=100, step=1),
                        style="default"
                    ),
                    bsCollapsePanel(
                        "Landscape",
                        numericInput("landscape_resmax", "Resmax", value=100, min=1, max=10000, step=1),
                        numericInput("landscape_xmax", "Xmax", value=5, min=1, max=100, step=1),
                        style="default"
                    ),
                    bsCollapsePanel(
                        "Linear",
                        numericInput("linear_buffer", "Buffer", value=20, min=1, max=1000, step=1),
                        numericInput("linear_resmax", "Resmax", value=22000, min=1, max=10000, step=1),
                        numericInput("linear_rankmax", "Rankmax", value=4, min=1, max=100, step=1),
                        numericInput("linear_xmax", "Xmax", value=3, min=1, max=100, step=1),
                        style="default"
                    ),
                    bsCollapsePanel(
                        "Lamp",
                        numericInput("lamp_resmax", "Resmax", value=100000000, min=1, max=1e10, step=1),
                        numericInput("lamp_xmax", "Xmax", value=1, min=1, max=100, step=1),
                        numericInput("lamp_ext", "Ext", value=100, min=1, max=100, step=1),
                        style="default"
                    )
                ),
                bsCollapsePanel(
                    "◯  Roost",
                    sliderInput(inputId="radius", label="Radius in meters", min=100, max=5000, value=1000),
                    # checkboxInput(inputId="showRadius", label="Show radius", value=TRUE),
                    # h4("Roost Coordinates"),
                    style="default"
                ),
                bsCollapsePanel(
                    "◿  Drawing",
                    actionButton(inputId="add_drawing", label="+"),
                    hr(id="horizolo"),
                    style="default"
                ),
                bsCollapsePanel(
                    "▦  Raster",
                    sliderInput(inputId="resolution", label="Resolution (metres per pixel)", min=1, max=50, value=5),
                    sliderInput(inputId="n_circles", label="Number of source circles", min=1, max=50, value=5),
                    actionButton(inputId="generate_res", label="Generate Resistance Maps"),
                    actionButton(inputId="generate_curr", label="Generate Current Map"),
                    downloadButton(outputId="download", label="Download"),
                    hr(id="horizolo2")
                ),
                bsCollapsePanel(
                    "⍰  Help",
                    hr(id="horizolo2")
                )
            ),
            # strong(p("Latitude")),
            # strong(p("Longitude")),
            div(id="latlon_display",
                    div(style="display: inline-block;vertical-align:top;width:49%", verbatimTextOutput(outputId="latitude")),
                    div(style="display: inline-block;vertical-align:top;width:49%", verbatimTextOutput(outputId="longitude"))
            ),
            # strong(p("Easting")),
            # strong(p("Northing")),
            div(id="eastingnorthing_display",
                    div(style="display: inline-block;vertical-align:top;width:49%", verbatimTextOutput(outputId="easting")),
                    div(style="display: inline-block;vertical-align:top;width:49%", verbatimTextOutput(outputId="northing"))
            )            
        ),
    )
)
