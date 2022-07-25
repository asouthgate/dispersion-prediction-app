library(R6)
library(leaflet)
library(shiny)
library(shinyBS)
library(raster)

source("circuitscape_app/drawing.R")
source("R/transform.R")


remove_height_param <- function(i) {
    removeUI(selector=paste0("div:has(> #HEIGHT", i, ")"))
}

insert_height_param <- function(i) {
    insertUI(
        selector = paste0("#NAMETEXT", i),
        where = "afterEnd",
        ui = sliderInput(inputId=paste0("HEIGHT", i), label="Height in meters:", min=0, max=100, value=10),
    )   
}

#' Transform drawings to correct coordinates for existing pipeline
transform_extra_geoms_27700 <- function(spdata) {

    logger::log_debug("Trying to get extra geoms now")
    data <- spdata$shapes


    if (length(spdata$shapes$buildings) > 0) {
        logger::log_debug("Attempting to get extra buildings")
        extra_buildings_t <- sp::spTransform(data$buildings, "+init=epsg:27700")
        spdata_t$shapes$buildings <- extra_buildings_t
    }

    extra_roads <- data$roads
    extra_roads_t <- NULL

    # TODO: DRY
    if (length(extra_roads) > 0) {
        logger::log_debug("Attempting to get extra roads")
        logger::log_debug(extra_roads)
        terra::crs(extra_roads) <- sp::CRS("+init=epsg:4326")
        logger::log_debug(extra_roads)
        extra_roads_t <- sp::spTransform(extra_roads, "+init=epsg:27700")
        logger::log_debug(extra_roads_t)
        spdata_t$shapes$roads <- extra_roads_t
    }

    extra_rivers <- data$rivers
    extra_rivers_t <- NULL

    if (length(extra_rivers) > 0) {
        logger::log_debug("Attempting to get extra rivers")
        logger::log_debug(extra_rivers)
        terra::crs(extra_rivers) <- sp::CRS("+init=epsg:4326")
        logger::log_debug(extra_rivers)
        extra_rivers_t <- sp::spTransform(extra_rivers, "+init=epsg:27700")
        logger::log_debug(extra_rivers_t)
        spdata_t$shapes$rivers <- extra_rivers_t
    }

    logger::log_debug("getting extra lights")
    extra_lights <- data$lights
    if (length(extra_lights) > 0) {
        eldf <- data.frame(x=extra_lights$x, y=extra_lights$y, z=extra_lights$z)
        converted_pts <- vector_convert_points(eldf, 4326, 27700)
        extra_lights_t <- converted_pts
        extra_lights_t$z <- unlist(spdata$z$lights)
    } else {
        extra_lights_t <- data.frame(x=c(), y=c(), z=c())
    }

    spdata_t$shapes$lights <- extra_lights_t

    logger::log_debug("Returning extras")
    # return(list(extra_buildings=extra_buildings_t, extra_roads=extra_roads_t, 
    #             extra_rivers=extra_rivers_t, extra_lights=extra_lights_t,
    #             zvals=spdata$z
    #         ))
    return(spdata_t)
}

# TODO: this class is too big
# TODO: move responsibility for some observers to individual drawing classes
#   another way to do this would be delegate UI/observer responsibilities elsewhere
# TODO: separate public and private interfaces
#' A collection of drawings on a map
#'
#' This class, when initialized with a session, input, and map reference,
#' will insert UI elements, observers, etc. to allow drawing by the user.
#' This widget is designed to be used within a bsCollapse.
#' 
#' The widget maintains a 'selected' drawing; the one that is selected at any time
#' is the one that is modified by the user.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords points, map
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @examples
#' ls <- DrawnPoints$new(1, "lights", 10)
#' @field n Number of vertices.
DrawingCollection <- R6Class("DrawingCollection",
    private=list(

        MAX_DRAWINGS = 500,
        drawings = list(),
        oi_selectors = list(),
        oi_collapses = list(),
        oi_sliders = list(),
        oi_deletors = list(),
        map_proxy = NULL,
        session = NULL,

        create_add_obs = function(input) {

            print(private$session)
            print(input)

            o <- observeEvent(input$add_drawing, {
                logger::log_info("Adding drawing...")
                self$create_new_drawing(input)
            })

            print(o)

        },

        # TODO: M: should not be the responsibility of the DrawingCollection;
        # probably the objects themselves, or a function

        #' Create a UI element corresponding to a given drawing
        create_ui_element = function(i) {

            panelname <- paste0("SHAPE", i)
            divname <- paste0("DIV", i)
            selectname <- paste0("SELECTOR", i)
            buttonname <- paste0("BUTTON", i)
            checkname <- paste0("CHECKBOX", i)
            textname <- paste0("NAMETEXT", i)

            return (
                div(id=divname,
                    div(style="display: inline-block;vertical-align:top;width:10%", checkboxInput(inputId=checkname, label="🖉", value=FALSE)),
                    div(style="display: inline-block;vertical-align:top;width:75%",
                        bsCollapsePanel(
                            "▶",
                            textInput(textname, label="Label", value = "", width = NULL, placeholder = NULL),
                            selectInput(selectname, "Type", c("Building", "River", "Road", "Lights", "Light String")),
                            style="default"
                        )
                    ),
                    div(style="display: inline-block;vertical-align:top;width:10%", actionButton(inputId=buttonname, label="x"))
                )
            )
        },

        #' TODO: unsure where this responsibility should lie
        #' Setup observers for a newly created drawing
        #' @param render_switch a reactiveVal to indicate if we should render
        create_observers = function(input, i) {

            logger::log_info("Creating observers for a drawing...")

            # TODO: duplication
            divname <- paste0("DIV", i)
            buttonname <- paste0("BUTTON", i)
            selectname <- paste0("SELECTOR", i)
            panelname <- paste0("SHAPE", i)
            checkname <- paste0("CHECKBOX", i)
            textname <- paste0("NAMETEXT", i)

            private$oi_selectors[[i]] <- observeEvent(input[[selectname]], {
                logger::log_info("Selector triggered for drawing")
                new_type <- gsub(" ", "", tolower(input[[selectname]]), fixed=TRUE)
                if (!is.null(new_type)) {
                    # delete drawing, make one of a new type, add that again
                    dr <- private$drawings[[as.character(i)]]
                    dr$clear_graphics(private$map_proxy)
                    old_xv <- dr$curr_xvals
                    old_yv <- dr$curr_yvals
                    old_height <- dr$height
                    old_type <- dr$type

                    if (old_type == "lightstring") {
                        dr$destroy_spacing_param()
                    }

                    logger::log_debug(paste("Creating a drawing of new type", new_type))

                    remove_height_param(i)

                    # delete the old one
                    if (new_type == "lightstring") {
                        private$drawings[[as.character(i)]] <- LightString$new(i, new_type, old_height)
                        insert_height_param(i)
                        private$drawings[[as.character(i)]]$insert_spacing_param_ui(input)
                    }
                    else if (new_type == "road" || new_type == "river") {
                        private$drawings[[as.character(i)]] <- DrawnLine$new(i, new_type, old_height)
                    }
                    else if (new_type == "lights") {
                        insert_height_param(i)
                        private$drawings[[as.character(i)]] <- DrawnPoints$new(i, new_type, old_height)
                    }
                    else {
                        insert_height_param(i)
                        private$drawings[[as.character(i)]] <- DrawnPolygon$new(i, new_type, old_height)
                        # private$drawings[[as.character(i)]] <- DrawnPolygon$new(paste0("polyLayer", self$n_drawings), new_type, old_height)
                    }
                }
            })
            logger::log_info("Created selector...")

            private$oi_collapses[[i]] <- observeEvent(input[[checkname]], {
                self$select(input[[checkname]], i)
            }, ignoreInit = TRUE)
            logger::log_info("Created collapse...")

            private$oi_sliders[[i]] <- observeEvent(input[[paste0("HEIGHT", i)]], {
                private$drawings[[as.character(i)]]$height <- as.double(input[[paste0("HEIGHT", i)]])
            })
            logger::log_info("Created slider...")

            private$oi_deletors[[i]] <- observeEvent(input[[buttonname]], {
                self$delete(divname, i)
            }, ignoreInit = TRUE, once = TRUE)
            logger::log_info("Created delete button...")
        }
    ),
    public= list(
        selected_i = NULL,
        n_created = 0,
        n_drawings = 0,

        initialize = function(input, session, map) {
            logger::log_info("Initializing drawing collection...")
            private$create_add_obs(input)
            private$map_proxy <- map
            private$session <- session
            print(session)
            print(private$session)

        },

        select = function(input_val, i) {
            logger::log_info("Selecting...")
            if (!input_val && is.null(self$selected_i)) {
                # box is not checked, and nothing is selected, we dont want this
                return()
            }
            if (!input_val && i != self$selected_i) {
                # not selected, and not currently selected, so if this is triggered, it is by something mysterious we dont want
                return()
            }
            if (is.null(self$selected_i)) {
                self$selected_i <- i
            } else if (self$selected_i == i) {
                self$selected_i <- NULL
            } else {
                updateCheckboxInput(private$session, paste0("CHECKBOX", self$selected_i), value = 0)
                self$selected_i <- i
            }
        },

        delete = function(divname, i) {
            logger::log_info("Deleting object")
            self$n_drawings <- self$n_drawings - 1
            removeUI(selector = paste0("#", divname))
            private$drawings[[as.character(i)]]$clear_graphics(private$map_proxy)
            private$drawings[[as.character(i)]] <- NULL
            if (is.null(self$selected_i) || self$selected_i == i) {
                self$selected_i <- NULL
            }
            private$oi_selectors[[i]]$destroy()
            private$oi_collapses[[i]]$destroy()
            private$oi_sliders[[i]]$destroy()
            private$oi_deletors[[i]]$destroy()
            if (self$n_drawings < private$MAX_DRAWINGS) {
                enable("add_drawing")
            }
        },

        write = function(shp_dir) {

            spatial_dfs <- self$get_spatial_dfs()

            buildings <- spatial_dfs$buildings
            roads <- spatial_dfs$roads
            rivers <- spatial_dfs$rivers
            lights <- spatial_dfs$lights

            if (!is.null(buildings)) {
                logger::log_info(paste("Writing buildings to", shp_dir))
                writeOGR(buildings, shp_dir, layer="buildings", driver="ESRI Shapefile", overwrite_layer=T)
            }
            if (!is.null(roads)) {
                writeOGR(roads, shp_dir, layer="roads", driver="ESRI Shapefile", overwrite_layer=T)
            }
            if (!is.null(rivers)) {
                writeOGR(rivers, shp_dir, layer="rivers", driver="ESRI Shapefile", overwrite_layer=T)
            }
            if (nrow(lights) > 0) {
                write.csv(lights, paste0(shp_dir, "/lights.csv"))
            }

        },

        read_buildings_file = function(f) {
            logger::log_info("Reading buildings")
            spdf <- readOGR(f, "buildings")
        },

        read_roads_file = function(f) {
            logger::log_info("Reading buildings")
            spdf <- readOGR(f, "buildings")
        },

        read_rivers_file = function(f) {
            logger::log_info("Reading buildings")
            spdf <- readOGR(f, "buildings")
        },

        read_lights_file = function(f) {
            logger::log_info("Reading buildings")
            spdf <- readOGR(f, "buildings")
        },

        get_buildings = function() {

            logger::log_debug("Getting buildings...")

            heights <- c()
            building_polygons <- list()

            bi <- 1

            for (d in private$drawings) {

                if (d$type == "building" && d$n >= 4) {
                    shape <- list(d$get_shape())
                    logger::log_debug("Creating a polygon...")
                    polygon <- Polygons(shape, paste0("b", bi))
                    logger::log_debug("Appending...")
                    building_polygons <- append(building_polygons, polygon)
                    logger::log_debug("Appending to heights")
                    heights <- c(heights, d$height)
                    bi <- bi + 1

                }

            }

            x <- length(building_polygons)

            if (x == 0) {
                return(NULL)
            }

            rownames <- paste0("b", 1:(bi-1))

            print(rownames)
            print(heights)

            d <- data.frame(row.names = rownames, heights=heights)

            logger::log_debug("Creating spatial polygons data frame...")
            spd <- SpatialPolygonsDataFrame(SpatialPolygons(building_polygons), data = d)
            logger::log_debug("Setting CRS...")
            terra::crs(spd) <- sp::CRS("+init=epsg:4326")
            return(spd)

        },

        get_lines = function(type) {
            logger::log_info("Creating lines...")

            heights <- c()
            lines <- list()

            for (d in private$drawings) {

                if (d$type == type) {
                    print(d$get_shape())
                    lines <- append(lines, d$get_shape())
                    heights <- c(heights, d$height)
                }
            }

            x <- length(lines)

            if (x == 0) {
                return(NULL)
            }

            d <- data.frame(row.names = c(type), id = 1)
            lines <- Lines(lines, type)
            logger::log_info("Creating SpatialLines")
            splines <- SpatialLines(list(lines))
            spd <- SpatialLinesDataFrame(splines, data = d)
            print(spd)
            terra::crs(spd) <- sp::CRS("+init=epsg:4326")
            return(spd)

        },

        get_roads = function() {
            logger::log_debug("Getting roads...")
            return(self$get_lines("road"))
        },

        get_rivers = function() {
            logger::log_debug("Getting rivers...")
            return(self$get_lines("river"))
        },

        get_lights = function() {

            logger::log_debug("Getting lights.")

            dfs <- list()

            for (d in private$drawings) {

                if (d$type == "lights" || d$type == "lightstring") {
                    dfs <- append(dfs, list(d$get_shape()))
                }
            }

            if (length(dfs) == 0) {
                return(data.frame(x=c(), y=c(), z=c()))
            }

            lightsdf <- do.call(rbind, dfs)

            return(lightsdf)

        },

        #' Retrieve spatial data associated with all drawings for use downstream
        get_spatial_dfs = function(crs=NULL) {

            logger::log_debug("Getting spatial data from drawings.")

            result <- list(buildings=self$get_buildings(), roads=self$get_roads(), 
                        rivers=self$get_rivers(), lights=self$get_lights())

            if (!is.null(crs)) {
                logger::log_debug("Transforming spatial data frames.")
                crstring <- paste0("+init=epsg:", crs)
                if (!is.null(result$buildings)) {
                    logger::log_debug("Transforming buildings...")
                    result$buildings <- sp::spTransform(result$buildings, crstring)
                }
                if (!is.null(result$rivers)) {
                    logger::log_debug("Transforming rivers...")
                    result$rivers <- sp::spTransform(result$rivers, crstring)
                }
                if (!is.null(result$roads)) {
                    result$roads <- sp::spTransform(result$roads, crstring)
                }
                if (nrow(result$lights) > 0) {
                    logger::log_info("Transforming lights")
                    result$lights <- vector_convert_points(result$lights, 4326, crs)
                }
            }

            return(result)
        },

        #' Append a point to a drawing if it is not already marked complete or is not null
        add_point_complete = function(x, y, zoom_level) {
            if (is.null(self$selected_i) || private$drawings[[as.character(self$selected_i)]]$is_complete) {
                return()
            }
            private$drawings[[as.character(self$selected_i)]]$append(private$map_proxy, x, y)
        },

        render_drawings = function(zoom_level) {
            for (i in names(private$drawings) ) {
                private$drawings[[i]]$add_to_map(private$map_proxy, 100 / zoom_level)
            }
        },

        get_selected_drawing = function() {
            dr <- private$drawings[[self$selected_i]]
            return(dr)
        },

        #' unselect all drawings
        unselect_all = function() {
            if (!is.null(self$selected_i)) {
                updateCheckboxInput(private$session, paste0("CHECKBOX", self$selected_i), value = 0)
                self$selected_i <- NULL
            }
        },

        create_new_drawing = function(input) {

            logger::log_info("Attempting to create a new object")
            if (self$n_drawings < private$MAX_DRAWINGS) {

                self$n_created <- self$n_created + 1
                self$n_drawings <- self$n_drawings + 1

                # create a shape
                logger::log_info("Creating a new object")
                private$drawings[[as.character(self$n_created)]] <- DrawnPolygon$new(self$n_created, "building")
                logger::log_info("Put new object into drawing array")

                insertUI(
                    selector = "#horizolo",
                    where = "afterEnd",
                    ui = private$create_ui_element(self$n_created)
                )

                # insert_height_param(self$n_created)
                logger::log_info("About to create observers")
                ob <- private$create_observers(input, self$n_created)

                if (self$n_drawings == private$MAX_DRAWINGS) {
                    disable("add_drawing")
                }

            }

        }
    )
)