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
library(shinybusy)

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

shiny::addResourcePath('www', './www')

ui <- fluidPage(
    useShinyjs(),


    div(class="outer",
        tags$head(
            includeCSS("./circuitscape_app/style.css")
        ),
        leafletOutput("map", width="100%", height="100%"),

        # Top panel
        # absolutePanel(id = "banner", class = "panel panel-default", fixed = TRUE,
        #     draggable = FALSE, top = "0%", left = "0%", right = "0%",
        #     bottom = "100%", style="justify-content:center; border-radius: 0px;background-color:#3a3a3d; border-color:#3a3a3d",
        #     strong(p("Bat flight line predictor", style="text-align: center; color:#ffffff;")),

        # ),

        titlePanel("Bat flight line predictor!"),
        
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
            draggable = FALSE, top = "0%", left = "auto", right = "0%",
            bottom = "0%", style = "justify-content:center; border-radius: 0px;",
            # width="auto",
            # width = 330, height = "auto",

            HTML("<h2>Bat Flight Line <br/> Predictor</h2>"),

            HTML("
             <ol>
                <li>Select a roost location and size</li>
                <li>Import street light data if available</li>
                <li>Draw buildings, roads, or street lights</li>
                <li>Generate resistance and/or current maps</li>
            </ol>"
            ),

            HTML('<h2 style="color: #ffd900">⚠</h2>'),
            HTML('<p style="color: #ffd900; text-align: center;">Data is currently only available for Wales</p>'),

            # h4(id="big-heading", PIP2, class="ascii-art"),
            div(style="display: inline-block;vertical-align:top;min-width:5vw"),

            tags$div(class = "header", style="width:100%"),

            withMathJax(), 

            bsCollapse(id="collapseParameters", open="collapsePanel",
                # bsCollapsePanel("🞻  Street Lights", style="default",
                #     fileInput("streetLightsFile", NULL, buttonLabel="Upload CSV", accept=c(".csv"),  multiple=TRUE)
                # ),#
                bsCollapsePanel("🗁 Load Data", style="default",
                    selectInput("upload_select_name", "Type", c("Buildings", "Rivers", "Roads", "Lights")),
                    fileInput("upload_file", NULL, buttonLabel="Upload", accept=c(".shp", ".csv"),  multiple=TRUE)
                ),
                bsCollapsePanel("⚙ Parameters (Advanced)",
                    HTML("<p style='color:#962a2a'> Warning: please read <a href='https://link.springer.com/article/10.1007/s10980-019-00953-1'>the paper.</a>
                    before altering these parameters. </p>"),
                    sliderInput(inputId="n_circles", label="Number of source circles", min=1, max=50, value=50),
                    style="default",
                    bsCollapsePanel(
                        "Road",
                        numericInput("road_buffer", "Buffer (metres)", value=200, min=1, max=1000, step=1),
                        numericInput("road_resmax", "Max resistance", value=10, min=1, max=10000, step=1),
                        numericInput("road_xmax", "$$X_{max}$$", value=5, min=1, max=10, step=1),
                        style="default"
                    ),
                    bsCollapsePanel(
                        "River",
                        numericInput("river_buffer", "Buffer (metres)", value=10, min=1, max=100, step=1),
                        numericInput("river_resmax", "Max resistance", value=2000, min=1, max=10000, step=1),
                        numericInput("river_xmax", "$$X_{max}$$", value=4, min=1, max=100, step=1),
                        style="default"
                    ),
                    bsCollapsePanel(
                        "Landscape",
                        # value of 11 used from report
                        numericInput("landscape_rankmax", "Max rank", value=8, min=1, max=100, step=1),
                        numericInput("landscape_resmax", "Max resistance", value=100, min=1, max=10000, step=1),
                        numericInput("landscape_xmax", "$$X_{max}$$", value=5, min=1, max=100, step=1),
                        style="default"
                    ),
                    bsCollapsePanel(
                        "Linear",
                        numericInput("linear_buffer", "Buffer (metres)", value=20, min=1, max=1000, step=1),
                        numericInput("linear_resmax", "Max resistance", value=22000, min=1, max=10000, step=1),
                        numericInput("linear_rankmax", "Max rank", value=4, min=1, max=100, step=1),
                        numericInput("linear_xmax", "$$X_{max}$$", value=3, min=1, max=100, step=1),
                        style="default"
                    ),
                    bsCollapsePanel(
                        "Lamp",
                        numericInput("lamp_resmax", "Max resistance", value=100000000, min=1, max=1e10, step=1),
                        numericInput("lamp_xmax", "$$X_{max}$$", value=1, min=1, max=100, step=1),
                        numericInput("lamp_ext", "Max radius (metres)", value=100, min=1, max=100, step=1),
                        style="default"
                    )
                ),
                bsCollapsePanel(
                    "◯  Roost",
                    sliderInput(inputId="radius", label="Radius in meters", step=50, min=100, max=5000, value=2500),
                    numericInput("latitude_input", label="Latitude", value=50.684, step=0.01),
                    numericInput("longitude_input", label="Longitude", value=-2.104, step=0.01),
                    strong(p("Easting")),
                    verbatimTextOutput(outputId="easting"),
                    strong(p("Northing")),
                    verbatimTextOutput(outputId="northing"),
                    # checkboxInput(inputId="showRadius", label="Show radius", value=TRUE),
                    # h4("Roost Coordinates"),
                    style="default"
                ),
                bsCollapsePanel(
                    "◿  Drawing",
                    actionButton(inputId="add_drawing", label="+"),
                    downloadButton(outputId="download_drawings", label="Download Drawings"),
                    # fileInput("streetLightsFile", NULL, buttonLabel="Upload CSV", accept=c(".RData"),  multiple=TRUE),
                    hr(id="horizolo"),
                    use_busy_spinner(spin = "fading-circle"),
                    style="default"
                ),
                bsCollapsePanel(
                    "▦  Generate",
                    sliderInput(inputId="resolution", label="Resolution (metres per pixel)", min=1, max=100, value=25),
                    actionButton(inputId="generate_res", label="Generate Resistance Maps"),
                    actionButton(inputId="generate_curr", label="Generate Current Map"),
                    downloadButton(outputId="download", label="Download"),
                    hr(id="horizolo2")
                ),
                bsCollapsePanel(
                    "⍰  Help",
                    # hr(id="horizolo2"),
                    div(id="help_div",
                        HTML("<p> <b>To upload a CSV file for lamp data</b> select <em>Street Lights</em>, and click the upload button. 
                        This data should have three columns x, y, and z. Lamp CSVs should only cover the study area. If you require an
                        extremely large number of lamps, contact the administrator. </p>"),
                        HTML("<p><b>To adjust parameters used for resistance map calculation</b>, select 
                        <em>Resistance Parameters</em>, followed by the parameter you wish to adjust.</p>"),
                        HTML("<p><b>To adjust the radius analysed</b>, select <em>Roost</em>. Note that, 
                        the larger the radius selected, the smaller the permissible resolution. If a
                        high resolution run is required for a large radius, please contact the administrators.</p>"),
                        HTML("<p><b>To draw buildings</b>, rivers, roads, or street lamps directly onto the map, 
                        select <em>Drawings</em>, click the add button, and select the tick box. 
                        Parameters for an individual component can be adjusted by selecting the 
                        panel. Note that the height of a building relative to lamp height will affect light occlusion.
                        A string of lights can be created by selecting light string, and the chosen spacing. </p>"),
                        HTML("<p><b>To generate a resistance map</b>, select <em>Raster</em>, select your chosen resolution 
                        and the number of source circles, and then click generate. Resistance maps can be downloaded 
                        without running circuitscape.</p>"),
                        HTML("<p>After resistance map generation, <b> to run circuitscape</b>, click Generate <em>Current Map</em>. 
                        Note that for high resolution images, this step can take several minutes to an hour. Please be patient.</p>"),
                        HTML("<p>For <b> more information </b> on a given feature, click the question mark help icons.</p>"),
                        HTML("<p>For information on the methods used by this tool, see
                         <a href='https://link.springer.com/article/10.1007/s10980-019-00953-1'>the paper.</a>"),
                        HTML("<p>Encountered a bug? Please submit an issue on the 
                        <a href='https://github.com/js01/dispersion-prediction-app/issues'>github</a> repo.</p>"),
                        style="display: inline-block;vertical-align:top;width:30vw"
                    )
                )
            ),
            # strong(p("Latitude")),
            # strong(p("Longitude")),
            # div(id="latlon_display",
            #         div(style="display: inline-block;vertical-align:top;width:49%", verbatimTextOutput(outputId="latitude")),
            #         div(style="display: inline-block;vertical-align:top;width:49%", verbatimTextOutput(outputId="longitude"))
            #         # div(style="display: inline-block;vertical-align:top;width:49%", numericInput(outputId="latitude")),
            #         # div(style="display: inline-block;vertical-align:top;width:49%", numericInput(outputId="longitude"))
            # ),
            # strong(p("Easting")),
            # strong(p("Northing")),
            # div(id="eastingnorthing_display",
            #         div(style="display: inline-block;vertical-align:top;width:49%", verbatimTextOutput(outputId="easting")),
            #         div(style="display: inline-block;vertical-align:top;width:49%", verbatimTextOutput(outputId="northing"))
            # ),
            # div(
            #     HTML("<img src='./images/logo.jpg' alt='Cardiff University Logo' width=20px height=20px/>")
            # )
            # img(src="logo.png", align = "right"),
            # HTML(
            # "<div style='display: block; height: 200px; margin-left: -20px; margin-right: -20px; background-color:#913d3d; border-color:#913d3d'>
            # <h6>  </h6>
            # <p>  </p>
            # </div>"),

            div(id="logos",
                # div(style="display: inline-block; vertical-align:top; width:49%",
                
                # div(style="display: inline-block; vertical-align:top; margin-right: 0px;",
                img(src = "./www/logo_hefcw_inv.png",
                    height = 50,
                    width = 300,
                    style = "display: block; margin: 10px auto;"
                ),
                
                div(style="display: block; margin-left: auto; margin-right: auto; text-align: center; vertical-align:top;",
                    img(src = "./www/logo_su.png",
                        height = 50,
                        width = 50,
                        style = "display: inline; margin: 10px auto;"
                    ),
                    img(src = "./www/logo_cu.svg",
                        height = 50,
                        width = 50,
                        style = "display: inline; margin: 10px auto;"
                    )
                )
                # div(style="display: inline-block; vertical-align:top;",
                #     img(src = "./www/logo_cu.svg",
                #         height = 50,
                #         width = 50,
                #         style = "display: block;"
                #     )
                # )
            ),

        ),

    )
)
