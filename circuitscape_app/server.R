server <- function(input, output) {

    create_st_point <- function(x, y) { st_point(c(as.numeric(x), as.numeric(y))) }

    # Convert coordinates from one EPSG coordinate system to another
    convertPoint <- function(x, y, sourceCRS, destinationCRS) {
        sourcePoint = create_st_point(x, y)
        sfc = st_sfc(sourcePoint, crs=sourceCRS)
        destinationPoint = st_transform(sfc, destinationCRS)
        st_coordinates(destinationPoint)
    }

    # Get the x coordinate of a reactive st_point
    x <- function(point) { point()[1] }

    # Get the y coordinate of a reactive st_point
    y <- function(point) { point()[2] }
    
    # Format coordinates to 3 decimal places
    formatCoordinate <- function(n) { format(n, digits=3, nsmall=3) }

    map <- reactive({
        leaflet() %>%
            addTiles() %>%
            setView(lng=-3.964, lat=50.494, zoom=13)
    })
    output$map <- renderLeaflet(map())

    # Get the coordinates of the clicked map point in EPSG:4326 (WSG84)
    clicked4326 <- reactive({
        mapClick <- input$map_click
        if (is.null(mapClick)) return()
        create_st_point(mapClick$lng, mapClick$lat)
    })

    # Convert the coordinates of the clicked map point to EPSG:27700 (BNG)
    clicked27700 <- reactive({
        req(clicked4326())
        convertPoint(x(clicked4326), y(clicked4326), 4326, 27700)
    })

    # Roost Coordinates
    output$easting <- renderText(formatCoordinate(x(clicked27700)))
    output$northing <- renderText(formatCoordinate(y(clicked27700)))
    output$longitude <- renderText(formatCoordinate(x(clicked4326)))
    output$latitude <- renderText(formatCoordinate(y(clicked4326)))

    # Add/update map marker and circle at the clicked map point
    observe({
        mapClick <- input$map_click
        if (is.null(mapClick)) return()
        leafletProxy("map") %>%
            clearMarkers() %>%
            clearShapes() %>%
            addMarkers(lng=mapClick$lng, lat=mapClick$lat)
        if (input$showRadius) addCircles(leafletProxy("map"), lng=mapClick$lng, lat=mapClick$lat, weight=1, radius=as.numeric(input$radius))
    })

    # Hide the radius circle when the checkbox is unchecked
    observeEvent(input$showRadius, {
        if (!input$showRadius) clearShapes(leafletProxy("map"))
    })

    # Upload street lights CSV file
    streetLightsData <- reactive({
        req(input$streetLightsFile)
        csv = vroom::vroom(input$streetLightsFile$datapath, delim = ",")
    })
    output$head <- renderTable({
        req(input$streetLightsFile)
        head(streetLightsData(), input$n)
    })

    observeEvent(input$addRaster, {
        print("ADD RASTER!")
        r <- raster("logCurrent.tif")
        crs(r) <- CRS("+init=epsg:27700")
        addRasterImage(leafletProxy("map"), r, colors="Spectral", opacity=1)
    })

    observeEvent(input$generate, {
        roost = c(x(clicked27700), y(clicked27700))
        radius = input$radius
        generate(roost, radius)
    })
}
