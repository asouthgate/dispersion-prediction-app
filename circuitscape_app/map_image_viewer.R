library(R6)
library(raster)
library(shiny)
library(logger)
library(leaflet)

source("R/transform.R")

#' @description R6 Class for an image viewer widget for leaflet
#'
#' Use to render circular images on a leaflet map, such as resistance map rasters,
#' or the circuitscape current raster.
#'
#' @export
#' @importFrom R6 R6Class
MapImageViewer <- R6Class("MapImageViewer",
    public=list(
        initialize = function(map_proxy) {
            logger::log_debug("Initializing map image viewer")
            private$map_proxy <- map_proxy
        },

        load_plain_rasters=function(input, session, lon, lat, radius, resistance_maps, disk) {

            logger::log_debug("Adding data to map image viewer")
            private$lon <- lon
            private$lat <- lat
            private$radius <- radius

            private$resistance_maps <- resistance_maps

            for (name in names(private$resistance_maps)) {
                terra::crs(private$resistance_maps[[name]]) <- sp::CRS("+init=epsg:27700")
            }

            map_names <- c(names(private$resistance_maps), "None")

            private$disk <- disk
            logger::log_debug("Finished building features raster")
            private$has_data <- TRUE

            logger::log_debug("Adding UI elements")

            private$map_names <- map_names

            insertUI(
                        selector = "#horizolo2",
                        where = "afterEnd",
                        ui=div(id="show_raster_select_div",
                            selectInput("show_raster_select", "Show raster", map_names)
                        )
                    )

            logger::log_debug("Got checkboxes")
            private$add_observer(input, session)
            logger::log_debug("Got observers")


        },
        #' Add a log current raster to the map
        add_current=function(session, log_current_map) {
            logger::log_debug("Adding current to map image viewer.")
            private$log_current_map <- log_current_map
            terra::crs(private$log_current_map) <- sp::CRS("+init=epsg:27700")
            shiny::updateSelectInput(session, "show_raster_select",
                choices=c('Log Current', private$map_names)
            )
        },
        #' Reset the map image viewer
        reset=function(session) {
            if (private$has_data) {
                private$clear_groups()
                private$obs$destroy()
                shiny::updateSelectInput(session, "show_raster_select", choices=private$map_names, selected="Log Total Resistance")
                shiny::removeUI(paste0("#", "show_raster_select_div"), immediate = TRUE)
                shiny::removeUI(selector="div:has(> #show_raster_select_div)", immediate = TRUE)
                private$log_current_map <- NULL
            }
        }
    ),
    private = list(
        has_data = FALSE,
        base_inputs_raster = NULL,
        lon = NULL,
        lat = NULL,
        radius = NULL,
        obs = NULL,
        map_names = NULL,
        resistance_map = NULL,
        resistance_maps = NULL,
        log_current_map = NULL,
        disk = NULL,
        debug_rasters = NULL,
        vector_features = NULL,
        raster_features = NULL,
        lamps = NULL,
        debug_boxes = NULL,
        initialized = FALSE,
        map_proxy = NULL,
        #' Add observer for selection box 
        add_observer = function(input, session) {
            private$obs <- observeEvent(input$show_raster_select, {
                logger::log_info("MIV observer triggered: show raster selected")
                if (!private$initialized) {
                    private$initialized <- TRUE
                } else {
                    private$clear_groups()
                }
                private$draw_edge()
                logger::log_info(paste("Selected a raster to draw...", input$show_raster_select))
                if (input$show_raster_select == "Inputs") {
                    private$draw_base_raster()
                } else if (input$show_raster_select == "Log Current") {
                    if (!is.null(private$log_current_map)) {
                        private$draw_log_current_map()
                    } else {
                        logger::log_info("Log current is null")
                    }
                } else if (input$show_raster_select == "None") {
                    # do nothing
                } else {
                    # have some other value, assuming the raster select is defined
                    private$draw_generic_map(private$resistance_maps[[input$show_raster_select]])
                }
                logger::log_info("MIV: finished drawing.")
            })
        },
        #' Draw a raster on the map
        draw_generic_map = function(r) {
            logger::log_debug("Drawing generic raster")
            leaflet::addRasterImage(private$map_proxy, r * private$disk, colors="YlGnBu", opacity=0.8, group="resistance_raster")
        },
        draw_log_current_map = function() {
            logger::log_debug("Drawing log current raster")
            ninf <- raster::values(private$log_current_map)
            ninf <- ninf[!is.infinite(ninf)]
            domain <- c(min(ninf), max(ninf))
            col <- colorNumeric(
                # "RdYlBu",
                "YlGnBu",
                domain,
                na.color = NA,
                alpha = FALSE,
                reverse = TRUE
            )
            leaflet::addRasterImage(private$map_proxy, private$log_current_map * private$disk, colors=col, opacity=0.8, group="resistance_raster")
        },
        draw_log_resistance_map = function(r) {
            logger::log_debug("Drawing log resistance raster")
            leaflet::addRasterImage(private$map_proxy, r * private$disk, colors="YlGnBu", opacity=0.8, group="resistance_raster")
        },
        draw_resistance_map = function(r) {
            logger::log_debug("Drawing resistance raster")
            leaflet::addRasterImage(private$map_proxy, r * private$disk, colors="YlGnBu", opacity=0.8, group="resistance_raster")
        },
        draw_base_raster = function() {

            logger::log_debug("Drawing base raster")

            leaflet::addRasterImage(private$map_proxy, private$vector_features, colors="YlGnBu", opacity=0.8, group="feature_raster")

            leaflet::addRasterImage(private$map_proxy, private$raster_features, colors="black", opacity=0.8, group="feature_raster")

            if (nrow(private$lamps) > 0) {
                pts <- vector_convert_points(private$lamps, 27700, 4326)
                addCircles(private$map_proxy, lng=pts$x, lat=pts$y, weight=1, radius=5, fillOpacity = 0.8, color ="#ffedc7", group="feature_raster_lights")
            }
        },
        draw_edge = function() {
            logger::log_info("Drawing edge")
            addCircles(private$map_proxy, lng=private$lon, lat=private$lat, weight=10, opacity=0.8, color="#6f85ff", radius=private$radius, group="circle_raster")
        },
        clear_groups = function() {
            logger::log_debug("Clearing map image viewer")
            clearGroup(private$map_proxy, "feature_raster")
            clearGroup(private$map_proxy, "circle_raster")
            clearGroup(private$map_proxy, "feature_raster_lights")
            clearGroup(private$map_proxy, "resistance_raster")
            logger::log_debug("Cleared map image viewer")
        }
    )
)