library(R6)
library(raster)
library(shiny)
library(logger)
library(leaflet)

MapImageViewer <- R6Class("MapImageViewer", 
    public=list(
        base_inputs_raster=NULL,
        lon=NULL,
        lat=NULL,
        radius=NULL,
        resistance_map=NULL,
        resistance_maps=NULL,
        log_current_map=NULL,
        disk=NULL,
        debug_rasters=NULL,
        vector_features=NULL,
        raster_features=NULL,
        lamps=NULL,
        initialized=FALSE,
        map_proxy=NULL,
        initialize=function(input, session, map_proxy, lon, lat, radius, base_inputs, resistance_maps, log_current_map) {

            logger::log_info("Initializing map image viewer")

            self$map_proxy <- map_proxy

            self$lon <- lon
            self$lat <- lat
            self$radius <- radius

            self$debug_rasters <- c(r_dsm=base_inputs$r_dsm, r_dtm=base_inputs$r_dtm, lcm_r=base_inputs$lcm_r, resistance_maps)
            for (name in names(self$debug_rasters)) {
                terra::crs(self$debug_rasters[[name]]) <- sp::CRS("+init=epsg:27700")
            }

            self$add_checkboxes(names(self$debug_rasters))
            self$add_observers(input, session)


            self$resistance_maps = resistance_maps
            resistance_map <- resistance_maps$total_res
            self$resistance_map <- resistance_map
            terra::crs(self$resistance_map) <- sp::CRS("+init=epsg:27700")
            self$log_current_map <- log_current_map
            terra::crs(self$log_current_map) <- sp::CRS("+init=epsg:27700")
            # self$current_map@extent <- self$resistance_map@extent
            r <- base_inputs$groundrast

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

            self$disk <- base_inputs$disk
            self$vector_features <- rr * base_inputs$disk
            self$raster_features <- r * base_inputs$disk
            self$lamps <- base_inputs$lamps
            logger::log_debug("Finished building features raster")

        },
        add_checkboxes=function(debug_boxes) {
            insertUI(
                        selector = "#horizolo2",
                        where = "afterEnd",
                        ui = selectInput("show_raster_select", "Show raster",
                            c("Inputs", "Total Resistance", "Log Total Resistance", "Log Current", "None", debug_boxes))
                    )
        },
        add_observers=function(input, session) {
            observeEvent(input$show_raster_select, {
                if (!self$initialized) {
                    self$initialized <- TRUE
                } else {
                    self$clear_groups()
                }
                if (input$show_raster_select == "Inputs") {
                    self$draw_base_raster()
                } else if (input$show_raster_select == "Total Resistance") {
                    self$draw_resistance_map()
                } else if (input$show_raster_select == "Log Total Resistance") {
                    self$draw_log_resistance_map()
                } else if (input$show_raster_select == "Log Current") {
                    self$draw_log_current_map()
                } else if (input$show_raster_select == "None") {
                    # do nothing
                }
                else {
                    # have some other value, assuming the raster select is defined
                    self$draw_generic_map(self$debug_rasters[[input$show_raster_select]])
                }
            })
        },
        draw_generic_map=function(v) {
            logger::log_debug("Drawing generic raster")
            leaflet::addRasterImage(self$map_proxy, v * self$disk, colors="YlGnBu", opacity=0.8, group="resistance_raster")
        },
        draw_log_current_map=function() {
            logger::log_debug("Drawing log current raster")
            domain <- c(min(values(self$log_current_map)), max(values(self$log_current_map)))
            col <- colorNumeric(
                "RdYlBu",
                domain,
                na.color = "#808080",
                alpha = FALSE,
                reverse = TRUE
            )
            leaflet::addRasterImage(self$map_proxy, self$log_current_map * self$disk, colors=col, opacity=1.0, group="resistance_raster")
        },
        draw_log_resistance_map=function() {
            logger::log_debug("Drawing log resistance raster")
            leaflet::addRasterImage(self$map_proxy, log(self$resistance_map + 1) * self$disk, colors="YlGnBu", opacity=0.8, group="resistance_raster")
        },
        draw_resistance_map=function() {
            logger::log_debug("Drawing resistance raster")
            leaflet::addRasterImage(self$map_proxy, self$resistance_map * self$disk, colors="YlGnBu", opacity=0.8, group="resistance_raster")
        },
        draw_base_raster=function() {

            logger::log_debug("Drawing base raster")

            leaflet::addRasterImage(self$map_proxy, self$vector_features, colors="YlGnBu", opacity=0.8, group="feature_raster")

            leaflet::addRasterImage(self$map_proxy, self$raster_features, colors="black", opacity=0.7, group="feature_raster")

            addCircles(self$map_proxy, lng=self$lon, lat=self$lat, weight=3, color="#314891", fillOpacity = 0.4, radius=self$radius, group="feature_raster")

            if (nrow(self$lamps) > 0) {
                pts <- vector_convert_points(self$lamps, 27700, 4326)
                addCircles(self$map_proxy, lng=pts[1,], lat=pts[2,], weight=1, radius=5, fillOpacity = 1.0, color ="#ffedc7", group="feature_raster_lights")
            }
        },
        clear_groups=function() {
            logger::log_debug("Clearing map image viewer")
            clearGroup(self$map_proxy, "feature_raster")
            clearGroup(self$map_proxy, "feature_raster_lights")
            clearGroup(self$map_proxy, "resistance_raster")
            # clearGroup("resistance_raster")
            # clearGroup("current_raster")
            logger::log_debug("Cleared map image viewer")
        }
    )
)