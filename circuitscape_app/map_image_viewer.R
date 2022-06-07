library(R6)
library(raster)
library(shiny)
library(logger)
library(leaflet)

MapImageViewer <- R6Class("MapImageViewer", 
    public=list(
        initialize=function(map_proxy) {
            logger::log_info("Initializing map image viewer")
            private$map_proxy <- map_proxy
        },
        add_initial_data=function(input, session, map_proxy, lon, lat, radius, base_inputs, resistance_maps) {

            logger::log_info("Adding datat to map image viewer")

            private$lon <- lon
            private$lat <- lat
            private$radius <- radius

            private$debug_rasters <- c(r_dsm=base_inputs$r_dsm, r_dtm=base_inputs$r_dtm, lcm_r=base_inputs$lcm_r, resistance_maps)
            for (name in names(private$debug_rasters)) {
                terra::crs(private$debug_rasters[[name]]) <- sp::CRS("+init=epsg:27700")
            }

            private$add_checkboxes(names(private$debug_rasters))
            logger::log_debug("Got checkboxes")

            private$add_observers(input, session)
            logger::log_debug("Got observers")

            private$resistance_maps = resistance_maps
            resistance_map <- resistance_maps$total_res
            private$resistance_map <- resistance_map
            terra::crs(private$resistance_map) <- sp::CRS("+init=epsg:27700")
            # private$current_map@extent <- private$resistance_map@extent
            r <- base_inputs$groundrast
            logger::log_debug("Set some vars")

            values(r)[is.na(values(r))] <- 0

            # remove the ground pixels from the current map, since the bats dont flow through them really, not of interest?
            ground_mask <- base_inputs$groundrast * 0
            values(ground_mask)[is.na(values(ground_mask))] <- 1

            if (length(base_inputs$buildingsvec) > 0) {
                logger::log_debug("rasterizing buildings too")
                brast <- raster::rasterize(base_inputs$buildingsvec, base_inputs$groundrast, background=0)
                values(brast) <- pmin(values(brast), 1)
                r <- r + brast
            }

            if (length(base_inputs$rivers) > 0) {
                logger::log_debug("rasterizing rivers too")
                riverrast <- raster::rasterize(base_inputs$rivers, base_inputs$groundrast, background=0)
                values(riverrast) <- pmin(values(riverrast), 1)
                r <- r + riverrast
            }

            if (length(base_inputs$roads) > 0) {
                logger::log_debug("rasterizing roads too")
                roadrast <- raster::rasterize(base_inputs$roads, base_inputs$groundrast, background=0)
                values(roadrast) <- pmin(values(roadrast), 1)
                r <- r + roadrast
            }

            rr <- base_inputs$r_dtm
            rr <- rr + base_inputs$r_dsm
            rr <- rr + base_inputs$lcm_r
            terra::crs(rr) <- sp::CRS("+init=epsg:27700")

            values(r)[values(r) != 1] <- NA
            terra::crs(r) <- sp::CRS("+init=epsg:27700")

            private$disk <- base_inputs$disk
            private$vector_features <- rr * base_inputs$disk
            private$raster_features <- r * base_inputs$disk
            private$lamps <- base_inputs$lamps
            print(private$lamps)
            logger::log_debug("Finished building features raster")

            private$has_data <- TRUE

        },
        add_current=function(session, log_current_map) {
            logger::log_info("Adding current to map image viewer.")
            private$log_current_map <- log_current_map
            terra::crs(private$log_current_map) <- sp::CRS("+init=epsg:27700")
            updateSelectInput(session, "show_raster_select", 
                choices=c("Inputs", "Total Resistance", "Log Total Resistance", "Log Current", "None", private$debug_boxes)
            )
        },
        reset=function() {
            if (private$has_data) {
                private$clear_groups()
                private$obs$destroy()
                removeUI(paste0("#", "show_raster_select_div"))
            }
        }
    ),
    private=list(
        has_data=FALSE,
        base_inputs_raster=NULL,
        lon=NULL,
        lat=NULL,
        radius=NULL,
        obs=NULL,
        resistance_map=NULL,
        resistance_maps=NULL,
        log_current_map=NULL,
        disk=NULL,
        debug_rasters=NULL,
        vector_features=NULL,
        raster_features=NULL,
        lamps=NULL,
        debug_boxes=NULL,
        initialized=FALSE,
        map_proxy=NULL,
        add_checkboxes=function(debug_boxes) {
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
        add_observers=function(input, session) {
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
                }
                else {
                    # have some other value, assuming the raster select is defined
                    private$draw_generic_map(private$debug_rasters[[input$show_raster_select]])
                }
                logger::log_info("MIV: finished drawing.")
            })
        },
        draw_generic_map=function(v) {
            logger::log_debug("Drawing generic raster")
            leaflet::addRasterImage(private$map_proxy, v * private$disk, colors="YlGnBu", opacity=0.8, group="resistance_raster")
        },
        draw_log_current_map=function() {
            logger::log_debug("Drawing log current raster")
            ninf <- values(private$log_current_map)
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
            leaflet::addRasterImage(private$map_proxy, private$log_current_map * private$disk, colors=col, opacity=1.0, group="resistance_raster")
        },
        draw_log_resistance_map=function() {
            logger::log_debug("Drawing log resistance raster")
            leaflet::addRasterImage(private$map_proxy, log(private$resistance_map + 1) * private$disk, colors="YlGnBu", opacity=0.8, group="resistance_raster")
        },
        draw_resistance_map=function() {
            logger::log_debug("Drawing resistance raster")
            leaflet::addRasterImage(private$map_proxy, private$resistance_map * private$disk, colors="YlGnBu", opacity=0.8, group="resistance_raster")
        },
        draw_base_raster=function() {

            logger::log_debug("Drawing base raster")

            leaflet::addRasterImage(private$map_proxy, private$vector_features, colors="YlGnBu", opacity=0.8, group="feature_raster")

            leaflet::addRasterImage(private$map_proxy, private$raster_features, colors="black", opacity=0.7, group="feature_raster")

            # addCircles(private$map_proxy, lng=private$lon, lat=private$lat, weight=3, color="#314891", fillOpacity = 0.4, radius=private$radius, group="feature_raster")

            if (nrow(private$lamps) > 0) {
                pts <- vector_convert_points(private$lamps, 27700, 4326)
                addCircles(private$map_proxy, lng=pts$x, lat=pts$y, weight=1, radius=5, fillOpacity = 1.0, color ="#ffedc7", group="feature_raster_lights")
            }
        },
        draw_edge=function() {
            addCircles(private$map_proxy, lng=private$lon, lat=private$lat, weight=5, color="#6f85ff", fillOpacity = 0.0, radius=private$radius, group="circle_raster")
        },
        clear_groups=function() {
            logger::log_debug("Clearing map image viewer")
            clearGroup(private$map_proxy, "feature_raster")
            clearGroup(private$map_proxy, "circle_raster")
            clearGroup(private$map_proxy, "feature_raster_lights")
            clearGroup(private$map_proxy, "resistance_raster")
            logger::log_debug("Cleared map image viewer")
        }
    )
)