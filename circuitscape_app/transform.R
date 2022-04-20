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
            self$curr_xvals <- c()
            self$curr_yvals <- c()
            invisible(self)
        },
        pop = function() {
            self$curr_xvals <- self$curr_xvals[-self$n]
            self$curr_yvals <- self$curr_yvals[-self$n]
            self$n <- self$n - 1
        },
        try_complete_polygon = function(snap_eps=0.0001) {
            # must be more than 3; 3 down already, and a 4th attempt, which may be intended to close if super close to first, complete instead
            if (self$n > 3 && !(self$complete)) {
                print(paste("trying to complete polygon with", self$n, "polys"))
                print(paste("first and last are", self$curr_xvals[1], self$curr_xvals[self$n], self$curr_yvals[1], self$curr_yvals[self$n]))
                tmpv <- c(self$curr_xvals[1] - self$curr_xvals[self$n],
                     self$curr_yvals[1] - self$curr_yvals[self$n])
                # vnorm <- norm(tmpv, type="2")
                print(paste("calling approx d with", tmpv))
                vnorm <- approx_metres(tmpv[1], tmpv[2])
                print(paste("distance", vnorm))
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
            dp$add_to_map(map, dot_radius=circle_radius)
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