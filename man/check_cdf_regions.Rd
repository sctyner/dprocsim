% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check-cdfs.R
\name{check_cdf_regions}
\alias{check_cdf_regions}
\title{Check that the given proportions of CDFs fall in the given regions}
\usage{
check_cdf_regions(cdf_regions, au = 0.1, bl = 0.1, bw = 0.5)
}
\arguments{
\item{cdf_regions}{A tibble. Output from \code{check_cdf_counts()}}

\item{au}{Numeric. A number between 0-1. What proportion of simulated CDFs should lie above the upper bound?}

\item{bl}{Numeric. A number between 0-1. What proportion of simulated CDFs should lie below the lower bound?}

\item{bw}{Numeric. A number between 0-1. What proportion of simulated CDFs should between the upper and lower bounds?}
}
\description{
Check that the given proportions of CDFs fall in the given regions
}
