% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dp_proc_sim.R
\name{draw_from_ecdf}
\alias{draw_from_ecdf}
\title{Simulate from a given empirical CDF.}
\usage{
draw_from_ecdf(ecdf, n)
}
\arguments{
\item{ecdf}{Function. The empirical cdf of the data of interest. Output from \code{\link[stats:edcf]{stats::edcf()}}.}

\item{n}{Integer. Number of observations to simulate}
}
\description{
A wrapper around \code{\link[stats:quantile]{stats::quantile()}}.
}
