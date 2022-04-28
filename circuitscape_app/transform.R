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

# TODO: make stuff private
#' Class to store data associated with user-defined/drawn polygon
#' 
#' @param j an integer to identify, should be unique
DrawnPolygon <- R6Class("DrawnPolygon", list(

        polylayerid = NULL,
        circlayerid = NULL,
        epsilon = 10,
        curr_xvals = c(),
        curr_yvals = c(),
        is_complete = FALSE,
        n = 0,

        initialize = function(j) {
            self$polylayerid <- paste0("polyLayer", j)
            self$circlayerid <- paste0("circLayer", j)
        },

        clear = function(x) {
            print("CLEARING")
            self$curr_xvals <- c()
            self$curr_yvals <- c()
            self$n <- 0
            self$is_complete = FALSE
            invisible(self)
        },

        pop = function() {
            self$curr_xvals <- self$curr_xvals[-self$n]
            self$curr_yvals <- self$curr_yvals[-self$n]
            self$n <- self$n - 1
        },

        try_complete_polygon = function(snap_eps=0.0001) {
            # must be more than 3; 3 down already, and a 4th attempt, which may be intended to close if super close to first, complete instead
            if (self$n > 3 && !(self$is_complete)) {
                tmpv <- c(self$curr_xvals[1] - self$curr_xvals[self$n],
                     self$curr_yvals[1] - self$curr_yvals[self$n])
                vnorm <- approx_metres(tmpv[1], tmpv[2])
                if (vnorm < snap_eps) {
                    self$pop()
                    self$add_point(self$curr_xvals[1], self$curr_yvals[1])
                    self$is_complete <- TRUE
                }
            }
        },

        add_point = function(x, y) {
            if (!self$is_complete) {
                self$n <- self$n + 1
                self$curr_xvals <- c(self$curr_xvals, x)
                self$curr_yvals <- c(self$curr_yvals, y)
            }
            invisible(self)
        },

        #' Add a point and attempt to complete polygon
        add_point_complete = function(map, x, y, zoom_level) {
            circle_radius <- (100/(zoom_level))
            self$add_point(x, y)
            self$try_complete_polygon(snap_eps=circle_radius)
            self$add_to_map(map, dot_radius=circle_radius)
            invisible(self)
        },

        get_polygon = function() {
            xym <- cbind(self$curr_xvals, self$curr_yvals)
            p <- Polygon(xym)
            p
        },

        clear_graphics = function(map) {
            clearGroup(map, self$circlayerid)
            removeShape(map, self$polylayerid)
        },

        add_to_map = function(map, dot_radius = 10) {
            if (length(self$curr_xvals > 0)) {
                if (self$is_complete) {
                    clearGroup(map, self$circlayerid)
                    addPolygons(map, data=self$get_polygon(), weight=1, fillColor="#2f3236", color="#2f3236", fillOpacity = 0.8, layerId=self$polylayerid)
                } else {
                    addCircleMarkers(map, lng=self$curr_xvals[self$n], lat=self$curr_yvals[self$n], weight=1, radius=dot_radius, fillOpacity=1, color = "#2f3236", opacity = 1, group=self$circlayerid)
                    addPolylines(map, data=cbind(self$curr_xvals, self$curr_yvals), weight=1, color='#2f3236', fillColor = "#2f3236", opacity = 1, layerId=self$polylayerid)
                }
            }
            invisible(self)
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
        delete = function(i) { 
            
        },
        add = function(i) {

        },

        #' add a point and render
        add_point_complete = function(map, x, y, zoom_level) {
            print(zoom_level)
            print(paste("Adding complete points selecting", self$selected_i))
            if (is.null(self$selected_i) || self$drawings[[as.character(self$selected_i)]]$is_complete) {
                return()
            }
            self$drawings[[as.character(self$selected_i)]]$add_point_complete(map, x, y, zoom_level)
        },

        render_drawings = function(map, zoom_level) {
            for (i in names(self$drawings) ) {
                self$drawings[[i]]$add_to_map(map, 100/zoom_level)
            }
        },

        get_selected_drawing = function() {
            # print("trying to get a selected drawing???")
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
                            selectInput(selectname, "type", c("road", "river", "building", "light")),
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

            observeEvent(input[[buttonname]], {
                removeUI(selector = paste0("#", divname))
                self$drawings[[as.character(i)]]$clear_graphics(map_proxy)
                self$drawings[[as.character(i)]] <- NULL
                if (is.null(self$selected_i) || self$selected_i == i) {
                    self$selected_i <- NULL
                }
                oi_selector$destroy()
            }, ignoreInit = TRUE, once = TRUE)
        },

        # TODO: move to an init
        #' Create the collection; init
        #' @param should_render a reactiveVal switch
        create = function (session, input, map_proxy) {
            print("creating drawing collection")
            observeEvent(input[["add_drawing"]], {
                self$n <- self$n + 1
                if (self$n < self$MAX_DRAWINGS) {
                    # create a shape
                    self$drawings[[as.character(self$n)]] <- DrawnPolygon$new(paste0("polyLayer", self$n))
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