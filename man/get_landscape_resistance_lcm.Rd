% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resistance.R
\name{get_landscape_resistance_lcm}
\alias{get_landscape_resistance_lcm}
\title{Generate landscape resistance from lcm}
\usage{
get_landscape_resistance_lcm(lcm, buildings, soft_surf, rankmax, resmax, xmax)
}
\arguments{
\item{lcm}{landscape cover map, gives the type of surface}

\item{buildings}{buildings raster}

\item{soft_surf}{}
}
\value{
resistance raster
}
\description{
Again, certain landscape features are preferred by bats.
Resistance is determined by rankings.
Resistance in interval [0, resmax]
}
