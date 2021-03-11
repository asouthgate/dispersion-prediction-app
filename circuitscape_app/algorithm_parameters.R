# !/usr/bin/env Rscript

# library(R6)

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

AlgorithmParameters <- R6Class(
    "AlgorithmParameters",
    public=list(
        roadResistance = NULL,
        riverResistance = NULL,
        landscapeResistance = NULL,
        linearResistance = NULL,
        lampResistance = NULL,
        initialize = function(roadResistance, riverResistance, landscapeResistance, linearResistance, lampResistance) {
            self$roadResistance = roadResistance
            self$riverResistance = riverResistance
            self$landscapeResistance = landscapeResistance
            self$linearResistance = linearResistance
            self$lampResistance = lampResistance
        }
    )
)

a = AlgorithmParameters$new(
    RoadResistance$new(buffer=200, resmax=10, xmax=5),
    RiverResistance$new(buffer=10, resmax=2000, xmax=4),
    LandscapeResistance$new(resmax=100, xmax=5),
    LinearResistance$new(buffer=10, resmax=22000, rankmax=4, xmax=3),
    LampResistance$new(resmax=1e8, xmax=1, ext=100)
)
print(paste("     Road:", a$roadResistance$buffer, a$roadResistance$resmax, a$roadResistance$xmax))
print(paste("    River:", a$riverResistance$buffer, a$riverResistance$resmax, a$riverResistance$xmax))
print(paste("Landscape:", a$landscapeResistance$resmax, a$landscapeResistance$xmax))
print(paste("   Linear:", a$linearResistance$buffer, a$linearResistance$resmax, a$linearResistance$rankmax, a$linearResistance$xmax))
print(paste("     Lamp:", a$lampResistance$resmax, a$lampResistance$xmax, a$lampResistance$ext))
