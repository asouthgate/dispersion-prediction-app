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
        epsilon = 10,
        curr_xvals = c(),
        curr_yvals = c(),
        is_complete = FALSE,
        n = 0,
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
            # removeShape(map, "DrawnPolygon")
        },
        add_to_map = function(map, dot_radius = 10) {
            # if (self$n > 0) {
            #     self$clear_graphics(map)
            # }
            #addCircles(map, lng=self$curr_xvals[i], lat=self$curr_yvals[i], weight=1, radius=dot_radius, layerId="DrawnPolygon")
            if (length(self$curr_xvals > 0)) {
                # clearShapes(map)
                for (i in 1:self$n) {
                    addCircles(map, lng=self$curr_xvals[i], lat=self$curr_yvals[i], weight=1, radius=dot_radius, color = "red")
                }
                if (self$is_complete) {
                    addPolylines(map, data=cbind(self$curr_xvals, self$curr_yvals), weight=1, color='red', fillColor = "red")
                    addPolygons(map, data=self$get_polygon(), weight=1, fillColor="dark grey", color="dark grey")
                } else {
                    addPolylines(map, data=cbind(self$curr_xvals, self$curr_yvals), weight=1, color='red', fillColor = "red")
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
        drawings = vector(mode = "list", length = 10),
        observers = vector(mode = "list", length = 10),
        n = 0,
        c = 0,
        delete = function(i) { 
            
        },
        add = function(i) {

        },
        create_ui_element = function(i) {
            panelname <- paste0("PANEL", i)
            divname <- paste0("DIV", i)
            selectname <- paste0("SELECTOR", i)
            buttonname <- paste0("BUTTON", i)
            return (
                div(id=divname,
                    div(style="display: inline-block;vertical-align:top;width:80%",
                        bsCollapsePanel(
                            panelname,
                            selectInput(selectname, "type", c("road", "river", "building", "light")),
                            style="default"
                        ),
                    ),
                    div(style="display: inline-block;vertical-align:top;width:10%", actionButton(inputId=buttonname, label="x"))
                )
            )
        },
        create_observer = function(input, i) {
            divname <- paste0("DIV", i)
            buttonname <- paste0("BUTTON", i)
            selectname <- paste0("SELECTOR", i)
            oi <- observeEvent(input[[selectname]], {
                print(paste("SELECTOR", i))
                print(input[[selectname]])
            })
            observeEvent(input[[buttonname]], {
                removeUI(selector = paste0("#", divname))
                # self$observers[i]$destroy()
                print(paste("destroyed", buttonname))
                oi$destroy()
            }, ignoreInit = TRUE, once = TRUE)
            # self$observers[i] <- oi
        },
        main = function (input) {
            observeEvent(input[["add_drawing"]], {
                self$n <- self$n + 1
                if (self$n < self$MAX_DRAWINGS) {
                    insertUI(
                        selector = "#add_drawing",
                        where = "afterEnd",
                        ui = self$create_ui_element(self$n)
                    )
                    ob = self$create_observer(input, self$n)
                }
            })
        }
    )
)