#' User interface for bat app
#'

library(leaflet)
library(shiny)
library(shinyBS)
library(shinyjs)

"     xxxxxxxxxxxx
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

# Need this so logos are loaded properly
shiny::addResourcePath("www", "./www")

ui <- fluidPage(

    # Needed for shinyjs functionality
    useShinyjs(),

    div(class = "outer",

        # Include style css for custom style; not all style is defined in there
        tags$head(
            includeCSS("./circuitscape_app/style.css")
        ),

        # Map itself
        leafletOutput("map", width = "100%", height = "100%"),

        titlePanel("Horseshoe bat flight line predictor!"),

        # This holds the main content of the panel
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
            draggable = FALSE, top = "0%", left = "auto", right = "0%",
            bottom = "0%", style = "justify-content:center; border-radius: 0px;",

            HTML("<h2>Horseshoe Bat<br/> Flight Line <br/> Predictor</h2>"),

            HTML("
             <ol>
                <li>Select a roost location and size</li>
                <li>Import street light data if available</li>
                <li>Draw buildings, roads, or street lights</li>
                <li>Generate resistance and/or current maps</li>
            </ol>"
            ),

            HTML('<h2 style="color: #ffd900">⚠</h2>'),
            # HTML('<p style="color: #ffd900; text-align: center;">Data is currently only available for Wales</p>'),
            HTML('<p style="color: #ffd900; text-align: center;">For large areas, numbers of lights, or complex shapes, 
            <br/> please be patient.<br/><br/> </p>'),

            # Required for tex rendering
            withMathJax(),

            # The main `collapse` menu
            bsCollapse(id = "collapseParameters", open = "collapsePanel",

                bsCollapsePanel("🗁 Load Street Lights", style = "default",
                    fileInput("streetLightsFile", NULL, buttonLabel = "Upload CSV", accept = c(".csv"),  multiple = TRUE)
                ),

                bsCollapsePanel("⚙ Parameters (Advanced)",

                    HTML("<p style='color:#962a2a'> Warning: please read 
                    <a href='https://link.springer.com/article/10.1007/s10980-019-00953-1'>the paper </a>
                    before altering these parameters, or leave as defaults. </p>"),

                    sliderInput(inputId = "n_circles", label = "Number of source circles", min = 1, max = 50, value = 50),

                    style = "default",

                    bsCollapsePanel(
                        "Road",
                        numericInput("road_buffer", "Buffer (metres)", value = 200, min = 1, max = 1000, step = 1),
                        numericInput("road_resmax", "Max resistance", value = 10, min = 1, max = 10000, step = 1),
                        numericInput("road_xmax", "$$X_{max}$$", value = 5, min = 1, max = 10, step = 1),
                        style = "default"
                    ),

                    bsCollapsePanel(
                        "River",
                        numericInput("river_buffer", "Buffer (metres)", value = 10, min = 1, max = 100, step = 1),
                        numericInput("river_resmax", "Max resistance", value = 2000, min = 1, max = 10000, step = 1),
                        numericInput("river_xmax", "$$X_{max}$$", value = 4, min = 1, max = 100, step = 1),
                        style = "default"
                    ),

                    bsCollapsePanel(
                        "Landscape",
                        numericInput("landscape_rankmax", "Max rank", value = 8, min = 1, max = 100, step = 1),
                        numericInput("landscape_resmax", "Max resistance", value = 100, min = 1, max = 10000, step = 1),
                        numericInput("landscape_xmax", "$$X_{max}$$", value = 5, min = 1, max = 100, step = 1),
                        style = "default"
                    ),

                    bsCollapsePanel(
                        "Linear",
                        numericInput("linear_buffer", "Buffer (metres)", value = 20, min = 1, max = 1000, step = 1),
                        numericInput("linear_resmax", "Max resistance", value = 22000, min = 1, max = 10000, step = 1),
                        numericInput("linear_rankmax", "Max rank", value = 4, min = 1, max = 100, step = 1),
                        numericInput("linear_xmax", "$$X_{max}$$", value = 3, min = 1, max = 100, step = 1),
                        style = "default"
                    ),

                    bsCollapsePanel(
                        "Lamp",
                        numericInput("lamp_resmax", "Max resistance", value = 100000000, min = 1, max = 1e10, step = 1),
                        numericInput("lamp_xmax", "$$X_{max}$$", value = 1, min = 1, max = 100, step = 1),
                        numericInput("lamp_ext", "Max radius (metres)", value = 100, min = 1, max = 100, step = 1),
                        style="default"
                    )
                ),

                # This panel is to allow lat and lon to be adjusted precisely
                # Easting/Northing is displayed, but not inputted
                bsCollapsePanel(
                    "◯  Roost",
                    sliderInput(inputId = "radius", label = "Radius in meters", step = 50, min = 100, max = 5000, value = 2500),
                    numericInput("latitude_input", label = "Latitude", value = 50.604, step = 0.01),
                    numericInput("longitude_input", label = "Longitude", value = -3.600, step = 0.01),
                    strong(p("Easting")),
                    verbatimTextOutput(outputId = "easting"),
                    strong(p("Northing")),
                    verbatimTextOutput(outputId = "northing"),
                    style = "default"
                ),

                bsCollapsePanel(
                    "◿  Drawing",
                    div(id = "file_transfer",
                        downloadButton(
                            outputId = "download_drawings",
                            label = "Download",
                            style = "display: inline-block;vertical-align:top;width:25%"
                        ),
                        div(fileInput(
                            "upload_file", NULL,
                            buttonLabel = "📤 Upload", accept = c(".zip"),
                            multiple = TRUE), style = "display: inline-block;vertical-align:top;width:74%"
                        )
                    ),
                    div(actionButton(inputId = "add_drawing", label = "+"), style = "margin: 0 auto;"),
                    # TODO: meaningfully name element
                    # This hr is where we will insert the drawings in the server code
                    hr(id = "horizolo"),
                    style = "default"
                ),

                bsCollapsePanel(
                    "▦  Generate",
                    sliderInput(inputId = "resolution", label = "Resolution (metres per pixel)", min = 1, max = 100, value = 25),
                    actionButton(inputId = "generate_res", label = "Generate Resistance Maps"),
                    actionButton(inputId = "generate_curr", label = "Generate Current Map"),
                    downloadButton(outputId = "download", label = "Download"),
                    # This hr is where we will insert the map drop down
                    hr(id = "horizolo2")
                ),

                bsCollapsePanel(
                    "⍰  Help",
                    div(id = "help_div",
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
                        style = "display: inline-block;vertical-align:top;width:30vw"
                    )
                )
            ),

            div(id = "logos",

                img(src = "./www/logo_hefcw.jpg",
                    height = 50,
                    width = 300,
                    style = "display: block; margin: 10px auto;"
                ),

                div(style = "display: block; margin-left: auto; margin-right: auto; text-align: center; vertical-align:top;",
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

            ),

        ),

    )
)
