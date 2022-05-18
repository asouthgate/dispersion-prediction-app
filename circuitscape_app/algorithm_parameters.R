#
# R6 Classes to represent the parameters of the bat dispersion algorithm.
# Objects of these classes are populated by values collected from the
# corresponding parts of the user interface.
#

#
# The easting and northing coordinates of the roost
#
Roost <- R6Class(
    "Roost",
    public=list(
        x = 0,
        y = 0,
        radius = 0,
        initialize = function(x, y, radius) {
            self$x = x
            self$y = y
            self$radius = radius
        }
    )
)

#
# The road resistance parameters
#
RoadResistance <- R6Class(
    "RoadResistance",
    public=list(
        buffer = 0,
        resmax = 0,
        xmax = 0,
        initialize = function(buffer, resmax, xmax) {
            self$buffer = buffer
            self$resmax = resmax
            self$xmax = xmax
        }
    )
)

#
# The river resistance parameters
#
RiverResistance <- R6Class(
    "RiverResistance",
    public=list(
        buffer = 0,
        resmax = 0,
        xmax = 0,
        initialize = function(buffer, resmax, xmax) {
            self$buffer = buffer
            self$resmax = resmax
            self$xmax = xmax
        }
    )
)

#
# The landcape resistance parameters
#
LandscapeResistance <- R6Class(
    "LandscapeResistance",
    public=list(
        resmax = 0,
        xmax = 0,
        initialize = function(resmax, xmax) {
            self$resmax = resmax
            self$xmax = xmax
        }
    )
)

#
# The linear resistance parameters
#
LinearResistance <- R6Class(
    "LinearResistance",
    public=list(
        buffer = 0,
        resmax = 0,
        rankmax = 0,
        xmax = 0,
        initialize = function(buffer, resmax, rankmax, xmax) {
            self$buffer = buffer
            self$resmax = resmax
            self$rankmax = rankmax
            self$xmax = xmax
        }
    )
)

#
# The streep light resistance parameters
#
LampResistance <- R6Class(
    "LampResistance",
    public=list(
        resmax = 0,
        xmax = 0,
        ext = 0,
        initialize = function(resmax, xmax, ext) {
            self$resmax = resmax
            self$xmax = xmax
            self$ext = ext
        }
    )
)

#
# A class to bring together all the algorithm parameters.
#
AlgorithmParameters <- R6Class(
    "AlgorithmParameters",
    public=list(
        roost = NULL,
        roadResistance = NULL,
        riverResistance = NULL,
        landscapeResistance = NULL,
        linearResistance = NULL,
        lampResistance = NULL,
        resolution = 1,
        initialize = function(roost, roadResistance=NULL, riverResistance=NULL, landscapeResistance=NULL, linearResistance=NULL, lampResistance=NULL, resolution=1) {
            self$roost = roost
            self$roadResistance = roadResistance
            self$riverResistance = riverResistance
            self$landscapeResistance = landscapeResistance
            self$linearResistance = linearResistance
            self$lampResistance = lampResistance
            self$resolution = resolution
        }
    )
)

