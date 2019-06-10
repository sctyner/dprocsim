#' Density Estimation for a DP Posterior
#'
#' @param x A numeric. Value in the domain at which to evaluate the posterior.
#' @param epsilon A numeric. Small value around which to estimate the density.
#' @param sims A tibble. Simulations from [dppost_sim2()] (preferred) or [dppost_sim()].
#' @inheritParams dppost_sim
#'
#' @description Compute density esimates for a value by computing difference in distribution values for a small interval around value of interest.
#'
#' @importFrom tibble tibble
#' @importFrom purrr map_lgl
#' @importFrom tidyr unnest
#' @importFrom dplyr filter
#'
#'
eval_dens <- function(x, epsilon, sims, ...) {
  passing_sims <- sims %>%
    unnest() %>%
    mutate(in_interval = map_lgl(locations, # column to pass
      in_range, # to this function
      fixed_value = x, epsilon = epsilon
    )) %>% # other args passed to the function
    filter(in_interval)

  dens_ests <- passing_sims %>%
    group_by(rep) %>%
    summarize(
      min_loc = locations[which.min(locations)],
      min_cdf = weCDF[which.min(locations)],
      max_loc = locations[which.max(locations)],
      max_cdf = weCDF[which.max(locations)]
    ) %>%
    mutate(dens_est = max_cdf - min_cdf) %>%
    pull(dens_est)
}

#' Check if a Value Falls in Interval
#'
#' @param location A numeric. Value to check.
#' @param fixed_value A numeric. Center of the interval.
#' @param epsilon A numeric. Half of interval width.
#'
#' @description Use this function if you want to perform interval density esimates by hand.
#'
#' @export
in_range <- function(location, fixed_value, epsilon) {
  lower <- fixed_value - epsilon
  upper <- fixed_value + epsilon
  if (location > lower & location <= upper) {
    return(TRUE)
  } else {
    FALSE
  }
}

#'
#'
#'
#'
