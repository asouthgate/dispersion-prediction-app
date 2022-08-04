library(R6)
library(leaflet)
library(shiny)
library(shinyBS)

get_panelname <- function(i) { paste0("SHAPE", i) }
get_divname <- function(i) { paste0("DIV", i) }
get_selectname <- function(i) { paste0("SELECTOR", i) }
get_buttonname <- function(i) { paste0("BUTTON", i) }
get_checkname <- function(i) { paste0("CHECKBOX", i) }
get_textname <- function(i) { paste0("NAMETEXT", i) }
get_eyename <- function(i) { paste0("EYECHECKBOX", i) }


#' Draw line on a given map given xvals, yvals, colors, and layer id
#'
#' @param map
#' @param xvals
#' @param yvals
#' @param color
#' @param dot_radius
#' @param circle_layer_id
#' @param line_layer_id
draw_line_on_map <- function(map, xvals, yvals, color, line_layer_id) {
    # logger::log_info(paste("Drawing line on map", line_layer_id))
    addPolylines(map, data=cbind(xvals, yvals), weight=2, color=color, fillColor=color, opacity=1, group=line_layer_id)
}

#' Draw dot markers on a given map given xvals, yvals, color, and layer id, with optional radius
#'
#' @param map
#' @param xvals
#' @param yvals
#' @param color
#' @param dot_radius
#' @param circle_layer_id
draw_dots_on_map <- function(map, xvals, yvals, color, circle_layer_id, dot_radius=5) {
    n <- length(xvals)
    addCircles(map, lng=xvals[n], lat=yvals[n], weight=10, radius=dot_radius, fillOpacity=1, color = color, opacity=1, group=circle_layer_id)
}

#' Base class for a shape drawn on a map
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords polygon, base
#' @return N/A, use like ABC
#' @format \code{\link{R6Class}} object.
#' @examples
#' N/A, use like ABC, see LightString derived class
#' @field n Number of vertices.
#' @field color Color for map rendering.
#' @field height Additional data for objects (most objects) with height.
#' @field is_complete flag indicating whether a polygon has been closed.
#' @field type an additional metadata string.
DrawnShapeBase <- R6Class("DrawnShapeBase",
    private = list(

        # ID
        j = NULL,

        # TODO: M: probably not this class' responsibility
        # Layer id to which this will be rendered
        # TODO: these are actually "groups", not "layers", rename
        polylayerid = NULL,
        circlayerid = NULL,
        linelayerid = NULL,

        # vectors for x, y positions
        curr_xvals = c(),
        curr_yvals = c(),

        # positions that were clicked
        clicked_xvals = c(),
        clicked_yvals = c(),

        # observers
        oi_selector = NULL,
        oi_checkbox = NULL,
        oi_slider = NULL,
        oi_deletor = NULL,

        # TODO: M: not this class' responsibility
        snap_radius = 10,

        # The UI component where insertion after happen
        ui_selector = NULL,

        # The usual pop operation, on the vertices of the shape
        pop = function() {
            x <- private$curr_xvals[-self$n]
            y <- private$curr_yvals[-self$n]
            private$curr_xvals <- private$curr_xvals[-self$n]
            private$curr_yvals <- private$curr_yvals[-self$n]
            self$n <- self$n - 1
            c(x, y)
        },

        # Add a vertex
        add_point = function(x, y) {
            # If the shape is complete, do nothing
            if (!self$is_complete) {
                self$n <- self$n + 1
                private$curr_xvals <- c(private$curr_xvals, x)
                private$curr_yvals <- c(private$curr_yvals, y)
            }
            invisible(self)
        },

        #' Should be implemented for derived classes
        #'
        #' Draw self onto a given map
        add_to_map = function(map) {
            logger::log_debug(paste0("DrawingBase adding to map with color", self$color))
        }

    ),
    public = list(
        is_complete = FALSE,
        type = NULL,
        n = 0,
        height = 0,
        color = NULL,
        disabled = FALSE,
        visible = TRUE,

        # TODO: ABC should not have init
        #' Initialize
        #'
        #' @param j an integer to identify, should be unique
        #' @param color
        #' @param type an optional type string
        #' @param height an optional height
        initialize = function(j, color, type = "", height = 0, ui_selector = '#drawing_collection_ui') {

            private$ui_selector <- ui_selector

            private$polylayerid <- paste0("polyLayer", j)
            private$circlayerid <- paste0("circLayer", j)
            private$linelayerid <- paste0("lineLayer", j)
            private$j <- j

            self$height <- height
            self$color <- color
            self$type <- type
        },

        set_visibility = function(map, b) {
            if (!b) {
                self$set_invisible(map)
                self$visible <- FALSE
            } else {
                self$set_visible(map)
                self$visible <- TRUE
            }
        },

        set_visible = function(map) {
            if (self$visible) {return()}
            logger::log_info("Setting visible")
            self$visible <- TRUE
            showGroup(map, private$polylayerid)
            showGroup(map, private$circlayerid)
            showGroup(map, private$linelayerid)
        },

        set_invisible = function(map) {
            if (!self$visible) {return()}
            logger::log_info("Setting invisible")
            self$visible <- FALSE
            logger::log_info("hiding polylayer")
            hideGroup(map, private$polylayerid)
            hideGroup(map, private$linelayerid)
            logger::log_info("hiding circlayer")
            hideGroup(map, private$circlayerid)
        },

        #' Should be implemented for derived classes
        #' 
        #' Retrieve a data representation of self
        get_shape = function() {
            
        },

        #' Create a UI element corresponding to a given drawing
        create_ui_element = function(selected, label) {

            i <- private$j

            panelname <- get_panelname(i)
            divname <- get_divname(i)
            selectname <- get_selectname(i)
            buttonname <- get_buttonname(i)
            checkname <- get_checkname(i)
            textname <- get_textname(i)
            eyename <- get_eyename(i)

            logger::log_info(paste("Drawing creating UI element", i))

            logger::log_info(paste("Drawing textInput name is", textname))

            logger::log_info(paste("Drawing type is", self$type))

            ui_el <- div(id=divname,
                        div(style="display: inline-block;vertical-align:top;width:10%", 
                            checkboxInput(inputId=checkname, label="🖉", value=FALSE)),
                        div(style="display: inline-block;vertical-align:top;width:10%", 
                            checkboxInput(inputId=eyename, label="👁", value=TRUE)),
                        div(style="display: inline-block;vertical-align:top;width:55%",
                            bsCollapsePanel(
                                "▶",
                                textInput(textname, label="Label", value = label, width = NULL, placeholder = NULL),
                                selectInput(selectname, "Type", c("Building", "River", "Road", "Lights", "Light String"), selected=selected),
                                style="default"
                            )
                        ),
                        div(style="display: inline-block;vertical-align:top;width:10%", actionButton(inputId=buttonname, label="x")),
                    style="width: 30vw"
                )

            insertUI(
                # selector = "#horizolo",
                selector = private$ui_selector,
                where = "afterEnd",
                ui = ui_el,
                immediate = TRUE
            )

            if (self$disabled) {
                disable(textname)
                disable(selectname)
                disable(checkname)
            }

        },

        insert_height_param = function() {
            logger::log_info("Inserting height param")
            sel <- paste0("#", get_textname(private$j))
            insertUI(
                selector = sel,
                where = "afterEnd",
                ui = sliderInput(inputId=paste0("HEIGHT", private$j), label="Height in meters:", min=0, max=100, value=self$height),
                immediate = TRUE
            )
        },

        #' Clear all graphics associated with this shape from the map
        clear_graphics = function(map) {
            clearGroup(map, private$circlayerid)
            clearGroup(map, private$polylayerid)
            clearGroup(map, private$linelayerid)
        },

        append = function(map, x, y) {
            private$add_point(x, y)
            private$add_to_map(map)
        },

        # Add a record of the points that were clicked; curr_xvals may be changed in other places so this is stored separately
        append_click_history = function(x, y) {
            if (!self$is_complete) {
                private$clicked_xvals <- c(private$clicked_xvals, x)
                private$clicked_yvals <- c(private$clicked_yvals, y)
            }
        },

        set_vals = function(xs, ys) {
            private$clicked_xvals <- xs
            private$curr_xvals <- xs
            private$clicked_yvals <- ys
            private$curr_yvals <- ys
        },

        get_clicked_xvals = function() {
            return(private$clicked_xvals)
        },

        get_clicked_yvals = function() {
            return(private$clicked_yvals)
        }
    )
)

#' A line drawn on a map, representing e.g. road
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords line, map
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @examples
#' ls <- DrawnLine$new(1, "road", 10)
#' @field n Number of vertices.
DrawnLine  <- R6Class("DrawnLine",
    inherit = DrawnShapeBase,
    private = list(
        add_to_map = function(map) {
            logger::log_debug(paste("DrawnLine", private$j, "adding to map with color", self$color))
            xvals <- private$curr_xvals
            yvals <- private$curr_yvals

            if (length(xvals) > 1000) {
                # Too many values
                showNotification("Too many lights to render a shape.")
            }
            if (length(private$curr_xvals > 0)) {
                draw_line_on_map(map, xvals, yvals, self$color, private$linelayerid)
            }
        }
    ),
    public = list(

        #' Initialize line
        #'
        #' @param j an integer tag
        #' @param type an optional type value, e.g. river, or road
        #' @param height an optional height value
        initialize = function(j, type = "", height = 0) {

            logger::log_debug(paste("Creating a Line", j, "of type", type))

            if (type == "road") {
                tmp_col <- "#585c5e"
            } else if (type == "river") {
                tmp_col <- "#3678b5"
            } else {
                tmp_col <- "#ffffff"
            }

            super$initialize(j, tmp_col, type, height)
        },

        get_shape = function() {
            xym <- cbind(private$curr_xvals, private$curr_yvals)
            p <- sp::Line(xym)
            p
        }

    )
)

#' A collection of points drawn on a map, representing e.g. street lamps
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
DrawnPoints  <- R6Class("DrawnPoints",
    inherit = DrawnShapeBase,
    private = list(
        add_to_map = function(map) {
            if (length(private$curr_xvals > 0)) {
                draw_dots_on_map(map, private$curr_xvals, private$curr_yvals, self$color, private$circlayerid)
            }
        }
    ),
    public = list(

        #' Initialize line
        #'
        #' @param j an integer tag
        #' @param type an optional type value, e.g. river, or road
        #' @param height an optional height value
        initialize = function(j, type = "", height = 0) {

            if (type == "lights") {
                tmp_col <- "#ffbd17"
            } else {
                tmp_col <- "#ffffff"
            }

            super$initialize(j, tmp_col, type, height)
        },

        get_shape = function() {
            xym <- cbind(private$curr_xvals, private$curr_yvals)
            p <- data.frame(x=private$curr_xvals, y= private$curr_yvals, z=self$height)
            p
        }

    )
)

#' A collection of points drawn on a map, representing e.g. street lamps, but with a height per point
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
DrawnPointsVariableHeights  <- R6Class("DrawnPoints",
    inherit = DrawnShapeBase,
    private = list(
        add_to_map = function(map) {
            if (length(private$curr_xvals > 0)) {
                draw_dots_on_map(map, private$curr_xvals, private$curr_yvals, self$color, private$circlayerid)
            }
        }
    ),
    public = list(

        heights=c(),

        #' Initialize points
        #'
        #' @param j an integer tag
        #' @param type an optional type value, e.g. river, or road
        #' @param height an optional height value
        initialize = function(j, type = "", heights = NULL) {

            self$disabled = TRUE

            if (type == "lights") {
                tmp_col <- "#ffbd17"
            } else {
                tmp_col <- "#ffffff"
            }

            self$heights <- heights

            super$initialize(j, tmp_col, type, heights)
        },

        get_shape = function() {
            xym <- cbind(private$curr_xvals, private$curr_yvals)
            p <- data.frame(x=private$curr_xvals, y= private$curr_yvals, z=self$heights)
            p
        },

        insert_height_param = function() {
            # do nothing, this has multiple height values
        }

    )
)

# TODO: there is polymorphism here, should use inheritance or similar
#' @description R6 Class representing a single drawn object
#'
#' @export
#' @importFrom R6 R6Class
DrawnPolygon <- R6Class("DrawnPolygon",
    inherit = DrawnShapeBase,
    private = list(

        try_complete_polygon = function(snap_eps) {
            logger::log_debug("Attempting to complete polygon")
            # Must be more than 3 points down already, and on a 4th attempt,
            # which may be intended to close if close to first point, complete instead
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
                    addPolygons(map, data=self$get_shape(), weight=1, fillColor=self$color, color=self$color, fillOpacity = 0.8, group=private$polylayerid)
                } else {
                    draw_dots_on_map(map, private$curr_xvals, private$curr_yvals, self$color, private$circlayerid)
                    draw_line_on_map(map, private$curr_xvals, private$curr_yvals, self$color, private$linelayerid)
                }
            }
        }

    ),

    public = list(

        #' Class to store data associated with user-defined/drawn polygon
        #'
        #' @param j an integer to identify, should be unique
        #' @param type a type identifier
        initialize = function(j, type, height = 0) {

            logger::log_debug(paste("Creating a new DrawnPolygon", j))

            if (type == "building") {
                tmp_color <- "#6b4235"
            } else {
                tmp_color <- "#ffffff"
            }

            super$initialize(j, tmp_color, type, height)

            logger::log_info("Finished initializing new DrawnPolygon")
        },

        # TODO: separate out rendering and drawing
        #' Add a point and attempt to complete polygon
        append = function(map, x, y) {
            private$add_point(x, y)
            if (self$type == "building") {
                private$try_complete_polygon(private$snap_radius)
            }
            private$add_to_map(map)
            invisible(self)
        },

        get_shape = function() {
            xym <- cbind(private$curr_xvals, private$curr_yvals)
            p <- sp::Polygon(xym)
            p
        }
    )
)

#' @description a string of lights
#'
#' @export
#' @importFrom R6 R6Class
LightString <- R6Class("LightString",
    inherit = DrawnShapeBase,

    private = list(
        spacing_ui_name = NULL,
        spacing_obs = NULL,
        add_to_map = function(map) {
            if (length(private$curr_xvals > 0)) {
                draw_dots_on_map(map, private$curr_xvals, private$curr_yvals, self$color, private$circlayerid)
                draw_line_on_map(map, private$curr_xvals, private$curr_yvals, self$color, private$linelayerid)
            }
            invisible(self)
        }
    ),

    public = list(

        type = NULL,
        n = 0,
        # height fot the lights
        height = 0,
        # spacing between lights on the string
        spacing = 50,
        # j = NULL,

        #' Class to store data associated with user-defined/drawn polygon
        #'
        #' @param j an integer to identify, should be unique
        #' @param type a type identifier
        initialize = function(j, type, height=0) {
            private$polylayerid <- paste0("polyLayer", j)
            private$circlayerid <- paste0("circLayer", j)
            private$linelayerid <- paste0("lineLayer", j)
            self$type <- type
            self$height <- height
            self$color <- "#ff9900"
            private$j <- j
        },

        destroy_spacing_param = function() {
            removeUI(selector = paste0("div:has(> #", private$spacing_ui_name, ")"))
            private$spacing_obs$destroy()
        },

        insert_spacing_param_ui = function(input) {
            logger::log_debug("Inserting spacing param UI elements")
            lab <- paste0("SPACING", private$j)
            private$spacing_ui_name <- lab
            si <- sliderInput(inputId=lab, label="Spacing:", min=0, max=200, value=50)
            insertUI(
                selector = paste0("#NAMETEXT", private$j),
                where = "afterEnd",
                ui = si,
                immediate = TRUE
            )
            private$spacing_obs <- observeEvent(input[[lab]], {
                self$spacing <- input[[lab]]
            })
        },

        #' Add a point and generate lights along the string
        append = function(map, x, y) {
            logger::log_debug("Appending to lightstring drawing")
            private$add_point(x, y)
            if (length(private$curr_xvals) > 1) {
                logger::log_info("Adding to lightstring...")
                lastx = private$curr_xvals[length(private$curr_xvals)-1]
                lasty = private$curr_yvals[length(private$curr_yvals)-1]
                private$pop()
                dx = x-lastx
                dy = y-lasty
                l <- approx_metres(dx, dy)
                LIGHTSTRING_EPS = self$spacing
                n_along = l / LIGHTSTRING_EPS

                for (li in 1:n_along) {
                    private$add_point(lastx + li * dx / n_along, lasty + li * dy / n_along)
                    private$add_to_map(map)
                }
            }
            private$add_to_map(map)
            invisible(self)
        },

        get_shape = function() {
            p <- data.frame(x=private$curr_xvals, y= private$curr_yvals, z=self$height)
            p
        }
    )
)