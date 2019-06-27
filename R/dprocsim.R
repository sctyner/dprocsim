#' `dprocsim` package
#'
#' Simulate from Dirichlet Process priors and posteriors, and estimate posterior densities.
#'
#' @docType package
#' @name dprocsim
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("locations", "weights",
                                                        "weights2", "in_interval",
                                                        "weCDF", "max_cdf",
                                                        "min_cdf", "dens_est", "x", "y", "monotone",
                                                        "bound", "method", "xgrid", "new.xs", "new.ys",
                                                        "M", "above", "below", "between", "check3", "draws",
                                                        "n", "region", "dist_above", "dist_below", "dist_between"))
