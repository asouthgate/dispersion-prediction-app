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

#' Approximate metres length for a lat, lon vector, works on small scales
approx_metres <- function(dlat, dlon) {
    dlat <- abs(dlat)
    dlon <- abs(dlon)
    print(paste("calculating distance", dlat, dlon))
    x <- dlat
    y <- (dlon) * cos((dlat) * 0.00872664626)
    return(1000 * 111.319 * sqrt(x*x + y*y))
}

# TODO: make stuff private
#' Class to store data associated with user-defined/drawn polygon
DrawnPolygon <- R6Class("DrawnPolygon", list(
        # A value that determines how close a point has to be to be to the start to 'finish'
        # the polygon
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
                # vnorm <- norm(tmpv, type="2")
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
            # if (self$n > 0) {
            #     self$clear_graphics(map)
            # }
            #addCircles(map, lng=self$curr_xvals[i], lat=self$curr_yvals[i], weight=1, radius=dot_radius, layerId="DrawnPolygon")
            if (length(self$curr_xvals > 0)) {
                # clearShapes(map)
                # for (i in 1:self$n) {
                #     addCircles(map, lng=self$curr_xvals[i], lat=self$curr_yvals[i], weight=1, radius=dot_radius, color = "#cc8f3f", layerId=paste0(self$circlayerid, i))
                # }
                print(paste("circlayerid", self$circlayerid))
                print(paste("polylayerid", self$polylayerid))
                
                if (self$is_complete) {
                    # addPolylines(map, data=cbind(self$curr_xvals, self$curr_yvals), weight=1, color='#cc8f3f', fillColor = "#cc8f3f", opacity = 1, layerId=self$polylayerid)
                    clearGroup(map, self$circlayerid)
                    addPolygons(map, data=self$get_polygon(), weight=1, fillColor="#cc8f3f", color="#cc8f3f", fillOpacity = 0.7, layerId=self$polylayerid)
                } else {
                    addCircles(map, lng=self$curr_xvals[self$n], lat=self$curr_yvals[self$n], weight=1, radius=dot_radius, color = "#cc8f3f", group=self$circlayerid)
                    addPolylines(map, data=cbind(self$curr_xvals, self$curr_yvals), weight=1, color='#cc8f3f', fillColor = "#cc8f3f", opacity = 1, layerId=self$polylayerid)
                }
            }
            invisible(self)
        }
    )
)

# TODO: separate public and private interfaces
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
        #' @param render_switch a reactiveVal to indicate if we should render
        create_observers = function(session, input, i, map_proxy) {
            # print(paste("creating observers for", i))
            divname <- paste0("DIV", i)
            buttonname <- paste0("BUTTON", i)
            selectname <- paste0("SELECTOR", i)
            panelname <- paste0("SHAPE", i)
            checkname <- paste0("CHECKBOX", i)

            oi_selector <- observeEvent(input[[selectname]], {
                # print(paste("SELECTOR", i))
                # print(input[[selectname]])
            })

            oi_collapse <- observeEvent(input[[checkname]], {
                print(paste("SHAPE CURRENTLY SELECTED:", self$selected_i))
                print(paste("got input:"))
                print(input[[checkname]])
                # print(paste("OBSERVER FOR:", i, "with val", input[[checkname]]))
                if (!input[[checkname]] && is.null(self$selected_i)) {
                    # box is not checked, and nothing is selected, we dont want this
                    return()
                }
                if (!input[[checkname]] && i != self$selected_i) {
                    # not selected, and not currently selected, so if this is triggered, it is by something mysterious we dont want
                    # just got unchecked, set to null
                    # print(paste("Unselecting", i))
                    # self$selected_i <- NULL
                    # print(paste("Not checked, doing nothing", i))
                    return()
                }
                if (is.null(self$selected_i)) {
                    print(paste("Selecting", i))
                    self$selected_i <- i
                    # do nothing else, just set
                } else if (self$selected_i == i) {
                    # otherwise if already ticked, do nothing and set to null, its unticked
                    print(paste("Unselecting", i))
                    self$selected_i <- NULL
                } else {
                    print(paste("Unselecting old", self$selected_i))
                    updateCheckboxInput(session, paste0("CHECKBOX", self$selected_i), value = 0)
                    print(paste("Selecting", i))
                    self$selected_i <- i
                }
            }, ignoreInit = TRUE)

            observeEvent(input[[buttonname]], {
                # print(paste("Deleting", i))
                # print(self$selected_i)
                removeUI(selector = paste0("#", divname))
                # self$observers[i]$destroy()
                self$drawings[[as.character(i)]]$clear_graphics(map_proxy)
                self$drawings[[as.character(i)]] <- NULL
                if (is.null(self$selected_i) || self$selected_i == i) {
                    self$selected_i <- NULL
                }
                oi_selector$destroy()
                # print("deleted")
                # render_switch(TRUE)
            }, ignoreInit = TRUE, once = TRUE)
            # self$observers[i] <- oi
        },
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