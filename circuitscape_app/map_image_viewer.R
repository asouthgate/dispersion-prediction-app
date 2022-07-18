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
        #' Add checkboxes to the user interface and observers
        #'
        #' @param input shiny input
        #' @param session shiny session
        add_ui = function(input, session) {
            logger::log_debug("Adding UI elements")
            private$add_selectinputs(names(private$debug_rasters))
            logger::log_debug("Got checkboxes")
            private$add_observer(input, session)
            logger::log_debug("Got observers")
        },
        #' Precompute images to then render on leaflet
        #' This function is separated out and returns data to satisfy the futures library
        #'
        #' @param lon
        #' @param lat
        #' @param radius radius for circular images
        #' @param base_inputs list of inputs retrieved from the database
        #' @param resistance_maps list of resistance maps from resistance pipeline
        precompute_images = function(lon, lat, radius, base_inputs, raster_inp, vector_inp, resistance_maps) {

            logger::log_info("Computing images for map")

            images <- list()

            # images$lon <- lon
            # images$lat <- lat
            # images$radius <- radius

            private$lon <- lon
            private$lat <- lat
            private$radius <- radius

            images$debug_rasters <- c(r_dsm=raster_inp$r_dsm, r_dtm=raster_inp$r_dtm, lcm_r=raster_inp$lcm_r, resistance_maps)
            for (name in names(images$debug_rasters)) {
                terra::crs(images$debug_rasters[[name]]) <- sp::CRS("+init=epsg:27700")
            }

            images$resistance_maps = resistance_maps
            resistance_map <- resistance_maps$total_res
            images$resistance_map <- resistance_map
            terra::crs(images$resistance_map) <- sp::CRS("+init=epsg:27700")
            r <- base_inputs$groundrast
            logger::log_debug("Set some vars")

            raster::values(r)[is.na(raster::values(r))] <- 0

            # remove the ground pixels from the current map, since the bats dont flow through them really, not of interest?
            ground_mask <- base_inputs$groundrast * 0
            raster::values(ground_mask)[is.na(raster::values(ground_mask))] <- 1

            if (length(vector_inp$buildingsvec) > 0) {
                logger::log_debug("rasterizing buildings too")
                brast <- raster::rasterize(vector_inp$buildingsvec, base_inputs$groundrast, background=0)
                raster::values(brast) <- pmin(raster::values(brast), 1)
                r <- r + brast
            }

            if (length(vector_inp$rivers) > 0) {
                logger::log_debug("rasterizing rivers too")
                riverrast <- raster::rasterize(vector_inp$rivers, base_inputs$groundrast, background=0)
                raster::values(riverrast) <- pmin(raster::values(riverrast), 1)
                r <- r + riverrast
            }

            if (length(vector_inp$roads) > 0) {
                logger::log_debug("rasterizing roads too")
                roadrast <- raster::rasterize(vector_inp$roads, base_inputs$groundrast, background=0)
                raster::values(roadrast) <- pmin(raster::values(roadrast), 1)
                r <- r + roadrast
            }

            rr <- raster_inp$r_dtm
            rr <- rr + raster_inp$r_dsm
            rr <- rr + raster_inp$lcm_r
            terra::crs(rr) <- sp::CRS("+init=epsg:27700")

            raster::values(r)[raster::values(r) != 1] <- NA
            terra::crs(r) <- sp::CRS("+init=epsg:27700")

            images$disk <- base_inputs$disk
            images$vector_features <- rr * base_inputs$disk
            images$raster_features <- r * base_inputs$disk
            images$lamps <- base_inputs$lamps
            logger::log_debug("Finished building features raster")

            images$has_data <- TRUE

            images
        },
        #' Load precomputed images from the previous step
        load_precomputed_images=function(lon, lat, radius, images) {

            logger::log_debug("Adding data to map image viewer")
            private$lon <- lon
            private$lat <- lat
            private$radius <- radius

            private$debug_rasters <- images$debug_rasters

            private$resistance_maps <- images$resistance_maps
            private$resistance_map <- images$resistance_map

            private$disk <- images$disk
            private$vector_features <- images$vector_features
            private$raster_features <- images$raster_features
            private$lamps <- images$lamps
            logger::log_debug("Finished building features raster")
            private$has_data <- TRUE

        },
        #' Add a log current raster to the map
        add_current=function(session, log_current_map) {
            logger::log_debug("Adding current to map image viewer.")
            private$log_current_map <- log_current_map
            terra::crs(private$log_current_map) <- sp::CRS("+init=epsg:27700")
            shiny::updateSelectInput(session, "show_raster_select", 
                choices=c("Inputs", "Total Resistance", "Log Total Resistance", "Log Current", "None", private$debug_boxes)
            )
        },
        #' Reset the map image viewer
        reset=function() {
            if (private$has_data) {
                private$clear_groups()
                private$obs$destroy()
                shiny::removeUI(paste0("#", "show_raster_select_div"))
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
        #' Add selectInput elements to the UI
        #'
        #' @param debug_boxes optional list of select options for showing debug maps
        add_selectinputs = function(debug_boxes) {
            logger::log_info("Adding selectors")
            private$debug_boxes = debug_boxes
            insertUI(
                        selector = "#horizolo2",
                        where = "afterEnd",
                        ui=div(id="show_raster_select_div",
                            selectInput("show_raster_select", "Show raster",
                                c("Inputs", "Total Resistance", "Log Total Resistance", "None", debug_boxes)
                            )
                        )
                    )
        },
        #' Add observer for selection box 
        add_observer = function(input, session) {
            private$obs <- observeEvent(input$show_raster_select, {
                if (!private$initialized) {
                    private$initialized <- TRUE
                } else {
                    private$clear_groups()
                }
                private$draw_edge()
                if (input$show_raster_select == "Inputs") {
                    private$draw_base_raster()
                } else if (input$show_raster_select == "Total Resistance") {
                    private$draw_resistance_map()
                } else if (input$show_raster_select == "Log Total Resistance") {
                    private$draw_log_resistance_map()
                } else if (input$show_raster_select == "Log Current") {
                    private$draw_log_current_map()
                } else if (input$show_raster_select == "None") {
                    # do nothing
                } else {
                    # have some other value, assuming the raster select is defined
                    private$draw_generic_map(private$debug_rasters[[input$show_raster_select]])
                }
                logger::log_info("MIV: finished drawing.")
            })
        },
        #' Draw a raster on the map
        draw_generic_map = function(r) {
            logger::log_debug("Drawing generic raster")
            # leaflet::addRasterImage(private$map_proxy, r, colors="YlGnBu", opacity=0.8, group="resistance_raster")
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
        draw_log_resistance_map = function() {
            logger::log_debug("Drawing log resistance raster")
            leaflet::addRasterImage(private$map_proxy, log(private$resistance_map + 1) * private$disk, colors="YlGnBu", opacity=0.8, group="resistance_raster")
        },
        draw_resistance_map = function() {
            logger::log_debug("Drawing resistance raster")
            leaflet::addRasterImage(private$map_proxy, private$resistance_map * private$disk, colors="YlGnBu", opacity=0.8, group="resistance_raster")
        },
        draw_base_raster = function() {

            logger::log_debug("Drawing base raster")

            print(private$vector_features)

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