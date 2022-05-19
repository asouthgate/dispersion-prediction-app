library(R6)
library(sp)

#' Create an extent given algorithm parameters with roost location
#'  and radius (side length)
#'
#' @param x
#' @param y
#' @param delta side length of the bounding box
#' @return raster::extent object
create_extent <- function(x, y, delta) {

    xmin <- x - delta
    xmax <- x + delta
    ymin <- y - delta
    ymax <- y + delta

    raster::extent(xmin, xmax, ymin, ymax)
}

#' Get approximate metres length for a dlat, dlon, works on small scales
#' 
#' @param dlat
#' @param dlon
approx_metres <- function(dlat, dlon) {
    dlat <- abs(dlat)
    dlon <- abs(dlon)
    x <- dlat
    y <- (dlon) * cos((dlat) * 0.00872664626)
    return(1000 * 111.319 * sqrt(x*x + y*y))
}

#' Draw line markers on a map with dots
#' 
#' @param map
#' @param xvals
#' @param yvals
#' @param color
#' @param dot_radius
#' @param circle_layer_id
#' @param line_layer_id
draw_line_on_map <- function(map, xvals, yvals, color, line_layer_id) {
    n <- length(xvals)
    addPolylines(map, data=cbind(xvals, yvals), weight=2, color=color, fillColor = color, opacity = 1, layerId=line_layer_id)
}

#' Draw dot markers on a map
#' 
#' @param map
#' @param xvals
#' @param yvals
#' @param color
#' @param dot_radius
#' @param circle_layer_id
draw_dots_on_map <- function(map, xvals, yvals, color, circle_layer_id, dot_radius=5) {
    n <- length(xvals)
    addCircles(map, lng=xvals[n], lat=yvals[n], weight=1, radius=dot_radius, fillOpacity=1, color = color, opacity=1, group=circle_layer_id)
}

# TODO: use enum for types
#' Class to store data associated with user-defined/drawn polygon
#' 
#' @param j an integer to identify, should be unique
#' @param type a type identifier
DrawnPolygon <- R6Class("DrawnPolygon",

    private = list(

        polylayerid = NULL,
        circlayerid = NULL,
        curr_xvals = c(),
        curr_yvals = c(),
        last_zoom_level = NULL,
        snap_radius = 10,
        color = "#2f3236",

        pop = function() {
            private$curr_xvals <- private$curr_xvals[-self$n]
            private$curr_yvals <- private$curr_yvals[-self$n]
            self$n <- self$n - 1
        },


        add_point = function(x, y) {
            if (!self$is_complete) {
                self$n <- self$n + 1
                private$curr_xvals <- c(private$curr_xvals, x)
                private$curr_yvals <- c(private$curr_yvals, y)
            }
            invisible(self)
        },

        try_complete_polygon = function(snap_eps) {
            logger::log_debug("Attempting to complete polygon")
            # must be more than 3; 3 down already, and a 4th attempt, which may be intended to close if super close to first, complete instead
            if (self$n > 3 && !(self$is_complete)) {
                tmpv <- c(private$curr_xvals[1] - private$curr_xvals[self$n],
                     private$curr_yvals[1] - private$curr_yvals[self$n])
                vnorm <- approx_metres(tmpv[1], tmpv[2])
                if (vnorm < snap_eps) {
                    private$pop()
                    private$add_point(private$curr_xvals[1], private$curr_yvals[1])
                    self$is_complete <- TRUE
                }
            }
        },

        add_to_map = function(map) {
            if (length(private$curr_xvals > 0)) {
                if (self$is_complete) {
                    clearGroup(map, private$circlayerid)
                    addPolygons(map, data=self$get_shape(), weight=1, fillColor=private$color, color=private$color, fillOpacity = 0.8, layerId=private$polylayerid)
                } else if (self$type == "building") {
                    draw_dots_on_map(map, private$curr_xvals, private$curr_yvals, private$color, private$circlayerid)
                    draw_line_on_map(map, private$curr_xvals, private$curr_yvals, private$color, private$polylayerid)
                } else if (self$type != "lights") {
                    draw_line_on_map(map, private$curr_xvals, private$curr_yvals, private$color, private$polylayerid)
                } else {
                    draw_dots_on_map(map, private$curr_xvals, private$curr_yvals, private$color, private$circlayerid)
                }
            }
            invisible(self)
        }
    ),

    public = list(

        is_complete = FALSE,
        type = NULL,
        n = 0,
        height = 0,

        initialize = function(j, type) {
            private$polylayerid <- paste0("polyLayer", j)
            private$circlayerid <- paste0("circLayer", j)
            self$type <- type
            if (type == "building") {
                private$color <- "#6b4235"
            } else if (type == "road") {
                private$color <- "#585c5e"
            } else if (type == "river") {
                private$color <- "#3678b5"
            } else {
                private$color <- "#ff9900"
            }
        },

        set_type = function(map, new_type, rad=10) {
            # TODO: handle zoom level differently
            self$type <- new_type
        },

        #' Add a point and attempt to complete polygon
        append = function(map, x, y) {
            logger::log_debug("Appending to drawing")
            private$add_point(x, y)
            if (self$type == "building") {
                private$try_complete_polygon(private$snap_radius)
            }
            private$add_to_map(map)
            invisible(self)
        },

        get_shape = function() {
            xym <- cbind(private$curr_xvals, private$curr_yvals)
            if (self$type == "building") {
                p <- sp::Polygon(xym)
            } else if (self$type == "road" || self$type == "river") {
                p <- sp::Line(xym)
            } else {
                p <- data.frame(x=private$curr_xvals, y= private$curr_yvals, z=self$height)
            }
            p
        },

        clear_graphics = function(map) {
            clearGroup(map, private$circlayerid)
            removeShape(map, private$polylayerid)
        }
    )
)

# TODO: separate public and private interfaces
#' A collection of drawings placed on a (leaflet) map, has methods for selecting, adding, removing, setting up listeners etc.
DrawingCollection <- R6Class("DrawingCollection", 
    list(
        MAX_DRAWINGS = 10,
        drawings = list(),
        observers = list(),
        selected_i = NULL,
        n = 0,
        c = 0,

        initialize = function(input, session) {
            # observeEvent(input$collapseParameters, {
            # # Unselect drawing
            #     self$unselect_all(session)
            # })
        },
        
        get_spatial_data = function() {

            logger::log_debug("Getting spatial data from drawings.")

            tmp <- list(building=list(), river=list(), road=list(), lights=list())

            for (d in self$drawings) {
                if (d$n > 0) {
                    logger::log_debug(paste("Appending drawing of type", d$type))
                    tmp[[d$type]] <- append(tmp[[d$type]], d$get_shape())
                }
            }

            # Now we must convert, to play nice with existing pipeline

            if (length(tmp$building) > 0) {
                logger::log_debug("Converting building list of polygons to Polygons")
                logger::log_debug(paste("There are", length(tmp$building), "buildings"))
                polygons <- list()
                for (i in 1:length(tmp$building)) {
                    polygons <- append(polygons, Polygons(tmp$building[i], paste0("building", i)))
                }
                tmp$building <- SpatialPolygons(polygons)
            }

            if (length(tmp$river) > 0) {
                logger::log_debug("Converting river list of Line to Lines")
                lines <- Lines(tmp$river, "some_rivers")
                tmp$river <- SpatialLines(list(lines))
            }

            if (length(tmp$road) > 0) {
                logger::log_debug("Converting road list of Line to Lines")
                lines <- Lines(tmp$road, "some_roads")
                tmp$road <- SpatialLines(list(lines))
            }

            logger::log_debug("Returning drawings:")
            return(tmp)
        },

        #' add a point and render
        add_point_complete = function(map, x, y, zoom_level) {
            if (is.null(self$selected_i) || self$drawings[[as.character(self$selected_i)]]$is_complete) {
                return()
            }
            self$drawings[[as.character(self$selected_i)]]$append(map, x, y)
        },

        render_drawings = function(map, zoom_level) {
            for (i in names(self$drawings) ) {
                self$drawings[[i]]$add_to_map(map, 100/zoom_level)
            }
        },

        get_selected_drawing = function() {
            dr <- self$drawings[[self$selected_i]]
            return(dr)
        },

        create_ui_element = function(i) {

            panelname <- paste0("SHAPE", i)
            divname <- paste0("DIV", i)
            selectname <- paste0("SELECTOR", i)
            buttonname <- paste0("BUTTON", i)
            checkname <- paste0("CHECKBOX", i)

            return (
                div(id=divname,
                    div(style="display: inline-block;vertical-align:top;width:5%", checkboxInput(inputId=checkname, label=NULL, value=FALSE)),
                    div(style="display: inline-block;vertical-align:top;width:75%",
                        bsCollapsePanel(
                            panelname,
                            selectInput(selectname, "type", c("building", "river", "road", "lights")),
                            sliderInput(inputId=paste0("HEIGHT", i), label="Height in meters:", min=0, max=100, value=10),
                            style="default"
                        )
                    ),
                    div(style="display: inline-block;vertical-align:top;width:10%", actionButton(inputId=buttonname, label="x"))
                )
            )
        },

        #' Setup observers that need to trigger on events
        #' @param render_switch a reactiveVal to indicate if we should render
        create_observers = function(session, input, i, map_proxy) {

            divname <- paste0("DIV", i)
            buttonname <- paste0("BUTTON", i)
            selectname <- paste0("SELECTOR", i)
            panelname <- paste0("SHAPE", i)
            checkname <- paste0("CHECKBOX", i)

            oi_selector <- observeEvent(input[[selectname]], {
                new_type <- input[[selectname]]
                if (!is.null(new_type)) {
                    # delete drawing, make one of a new type, add that again
                    dr <- self$drawings[[as.character(i)]]
                    dr$clear_graphics(map_proxy)
                    old_xv <- dr$curr_xvals
                    old_yv <- dr$curr_yvals
                    # delete the old one
                    self$drawings[[as.character(i)]] <- DrawnPolygon$new(paste0("polyLayer", self$n), new_type)
                }
            })

            oi_collapse <- observeEvent(input[[checkname]], {
                if (!input[[checkname]] && is.null(self$selected_i)) {
                    # box is not checked, and nothing is selected, we dont want this
                    return()
                }
                if (!input[[checkname]] && i != self$selected_i) {
                    # not selected, and not currently selected, so if this is triggered, it is by something mysterious we dont want
                    return()
                }
                if (is.null(self$selected_i)) {
                    self$selected_i <- i
                } else if (self$selected_i == i) {
                    self$selected_i <- NULL
                } else {
                    updateCheckboxInput(session, paste0("CHECKBOX", self$selected_i), value = 0)
                    self$selected_i <- i
                }
            }, ignoreInit = TRUE)

            oi_slider <- observeEvent(input[[paste0("HEIGHT", i)]], {
                self$drawings[[as.character(i)]]$height <- input[[paste0("HEIGHT", i)]]
            })

            observeEvent(input[[buttonname]], {
                removeUI(selector = paste0("#", divname))
                self$drawings[[as.character(i)]]$clear_graphics(map_proxy)
                self$drawings[[as.character(i)]] <- NULL
                if (is.null(self$selected_i) || self$selected_i == i) {
                    self$selected_i <- NULL
                }
                oi_selector$destroy()
                oi_collapse$destroy()
                oi_slider$destroy()
            }, ignoreInit = TRUE, once = TRUE)
        },

        unselect_all = function(session) {
            updateCheckboxInput(session, paste0("CHECKBOX", self$selected_i), value = 0)
            self$selected_i <- NULL
        },

        # TODO: move to an init
        #' Create the collection; init
        #' @param should_render a reactiveVal switch
        create = function (session, input, map_proxy) {
            observeEvent(input[["add_drawing"]], {
                self$n <- self$n + 1
                if (self$n < self$MAX_DRAWINGS) {
                    # create a shape
                    self$drawings[[as.character(self$n)]] <- DrawnPolygon$new(paste0("polyLayer", self$n), "building")
                    insertUI(
                        selector = "#horizolo",
                        where = "afterEnd",
                        ui = self$create_ui_element(self$n)
                    )
                    ob = self$create_observers(session, input, self$n, map_proxy)
                }
            })
        }
    )
)