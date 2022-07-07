library(R6)
library(leaflet)
library(shiny)
library(shinyBS)
library(raster)

source("circuitscape_app/drawing.R")

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
    list(
        MAX_DRAWINGS = 50,
        drawings = list(),
        observers = list(),
        selected_i = NULL,
        n_created = 0,
        n_drawings = 0,
        c = 0,

        initialize = function(session, input, map) {
            self$create(input, session, map)
        },

        #' Retrieve spatial data associated with all drawings for use downstream
        get_spatial_data = function() {

            logger::log_debug("Getting spatial data from drawings.")

            tmp <- list(building=list(), river=list(), road=list(), lights=list())
            heights <- list(building=list(), lights=list(), road=list(), river=list())

            for (d in self$drawings) {
                if (d$n > 0) {
                    logger::log_debug(paste("Appending drawing of type", d$type))
                    tmp[[d$type]] <- append(tmp[[d$type]], list(d$get_shape()))
                    heights[[d$type]] <- append(heights[[d$type]], d$height)
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

            if (length(tmp$lights) > 0) {
                tmp$lights = do.call(rbind, tmp$lights)
                heights$lights <- tmp$lights$z
            }

            logger::log_debug("Returning drawings:")
            return(list(xy=tmp, z=heights))
        },

        #' Append a point to a drawing if it is not already marked complete or is not null
        add_point_complete = function(map, x, y, zoom_level) {
            if (is.null(self$selected_i) || self$drawings[[as.character(self$selected_i)]]$is_complete) {
                return()
            }
            self$drawings[[as.character(self$selected_i)]]$append(map, x, y)
        },

        render_drawings = function(map, zoom_level) {
            for (i in names(self$drawings) ) {
                self$drawings[[i]]$add_to_map(map, 100 / zoom_level)
            }
        },

        get_selected_drawing = function() {
            dr <- self$drawings[[self$selected_i]]
            return(dr)
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
                    div(style="display: inline-block;vertical-align:top;width:5%", checkboxInput(inputId=checkname, label=NULL, value=FALSE)),
                    div(style="display: inline-block;vertical-align:top;width:75%",
                        bsCollapsePanel(
                            "▼",
                            textInput(textname, label="label", value = "", width = NULL, placeholder = NULL),
                            selectInput(selectname, "type", c("building", "river", "road", "lights", "lightstring")),
                            sliderInput(inputId=paste0("HEIGHT", i), label="Height in meters:", min=0, max=100, value=10),
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
        create_observers = function(session, input, i, map_proxy) {

            # TODO: duplication
            divname <- paste0("DIV", i)
            buttonname <- paste0("BUTTON", i)
            selectname <- paste0("SELECTOR", i)
            panelname <- paste0("SHAPE", i)
            checkname <- paste0("CHECKBOX", i)
            textname <- paste0("NAMETEXT", i)

            oi_selector <- observeEvent(input[[selectname]], {
                new_type <- input[[selectname]]
                if (!is.null(new_type)) {
                    # delete drawing, make one of a new type, add that again
                    dr <- self$drawings[[as.character(i)]]
                    dr$clear_graphics(map_proxy)
                    old_xv <- dr$curr_xvals
                    old_yv <- dr$curr_yvals
                    old_height <- dr$height

                    logger::log_debug(paste("Creating a drawing of new type", new_type))

                    # delete the old one
                    if (new_type == "lightstring") {
                        self$drawings[[as.character(i)]] <- LightString$new(self$n_drawings, new_type, old_height)
                        self$drawings[[as.character(i)]]$insert_spacing_param_ui(input)
                    }
                    else if (new_type == "road" || new_type == "river") {
                        self$drawings[[as.character(i)]] <- DrawnLine$new(self$n_drawings, new_type, old_height)
                    }
                    else if (new_type == "lights") {
                        self$drawings[[as.character(i)]] <- DrawnPoints$new(self$n_drawings, new_type, old_height)
                    }
                    else {
                        self$drawings[[as.character(i)]] <- DrawnPolygon$new(self$n_drawings, new_type, old_height)
                        # self$drawings[[as.character(i)]] <- DrawnPolygon$new(paste0("polyLayer", self$n_drawings), new_type, old_height)
                    }
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
                self$drawings[[as.character(i)]]$height <- as.double(input[[paste0("HEIGHT", i)]])
            })

            observeEvent(input[[buttonname]], {
                self$n_drawings <- self$n_drawings - 1
                removeUI(selector = paste0("#", divname))
                self$drawings[[as.character(i)]]$clear_graphics(map_proxy)
                self$drawings[[as.character(i)]] <- NULL
                if (is.null(self$selected_i) || self$selected_i == i) {
                    self$selected_i <- NULL
                }
                oi_selector$destroy()
                oi_collapse$destroy()
                oi_slider$destroy()
                if (self$n_drawings < self$MAX_DRAWINGS) {
                    enable("add_drawing")
                }
            }, ignoreInit = TRUE, once = TRUE)
        },

        #' unselect all drawings
        unselect_all = function(session) {
            if (!is.null(self$selected_i)) {
                updateCheckboxInput(session, paste0("CHECKBOX", self$selected_i), value = 0)
                self$selected_i <- NULL
            }
        },

        #' Create the collection; init
        create = function (session, input, map_proxy) {
            observeEvent(input[["add_drawing"]], {

                logger::log_info("Attempting to create a new object")
                if (self$n_drawings < self$MAX_DRAWINGS) {
                    self$n_created <- self$n_created + 1
                    self$n_drawings <- self$n_drawings + 1
                    # create a shape
                    logger::log_info("Creating a new object")
                    self$drawings[[as.character(self$n_created)]] <- DrawnPolygon$new(self$n_created, "building")
                    insertUI(
                        selector = "#horizolo",
                        where = "afterEnd",
                        ui = self$create_ui_element(self$n_created)
                    )

                    ob = self$create_observers(session, input, self$n_created, map_proxy)

                    if (self$n_drawings == self$MAX_DRAWINGS) {
                        disable("add_drawing")
                    }

                }
            })
        }
    )
)