% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drawn-cdf-sim.R
\name{drawn_cdf_sim}
\alias{drawn_cdf_sim}
\title{Simulate from a drawn CDF}
\usage{
drawn_cdf_sim(shinydat, xgrid, ubnd = "bound 1", lbnd = "bound 2",
  meth)
}
\arguments{
\item{shinydat}{Data frame. Output from the shiny app containing information on the two sets of clicked
bounds, the two smoothed functions, and the resulting mean function.}

\item{xgrid}{Vector. Sequence of domain values at which to evaluate the CDF.}

\item{ubnd}{Character. Name of upper bound. Either \code{"bound 1"} or \code{"bound 2"}.}

\item{lbnd}{Character. Name of lower bound. Either \code{"bound 1"} or \code{"bound 2"}.}

\item{meth}{Character. Approximation method. either \code{"splines"} for \code{smooth.spline()} or \code{"linear"} for \code{approxfun()}}
}
\description{
After using the shiny app, \code{dprocsim::run_shiny()}, take the downloaded data and simulate from the resulting mean CDF value.
}
