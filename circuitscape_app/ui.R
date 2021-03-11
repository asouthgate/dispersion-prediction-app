ui <- fluidPage(
    useShinyjs(),

    tags$head(
        includeCSS("style.css")
    ),
    
    titlePanel("Bat Dispersion"),

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
            checkboxInput(inputId="showRadius", label="Show radius", value=TRUE),

            h3("Street Lighting"),
            fileInput("streetLightsFile", NULL, buttonLabel = "Upload CSV", accept=c(".csv"),  multiple=TRUE),
            tableOutput("head"),

            actionButton(inputId="generate", label="Generate Raster"),
            actionButton(inputId="download", label="Download Raster")

            # numericInput("n", "Rows", value=5, min=1, step=1),
        ),
        
        mainPanel(
            fillPage(leafletOutput("map"))
        )
    )
    
)
