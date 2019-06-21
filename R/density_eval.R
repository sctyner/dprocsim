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
#' @export
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

#' Augment a CDF to full domain and range
#'
#' @param dat Data frame. The CDF information from `dprocsim::run_shiny()`. Should have columns named `x`, `y`, `bound`, `method`.
#' @param xrange Numeric. Vector of length 2 giving the minimum and maximum of the function's domain.
#' @param yrange Numeric. Vector of length 2 giving the minimum and maximum of the fundtion's range. Should be `0:1` for CDF (default).
#'
#' @importFrom dplyr add_row
#' @export
augment_cdf <- function(dat,xrange, yrange = 0:1) {
  dat2 <-  dat %>% arrange(x) %>% mutate(monotone = y == cummax(y)) %>% filter(monotone)

  bnd <- unique(dat$bound)

  hits_ymin <- min(dat2$y) == yrange[1]
  hits_ymax <- max(dat2$y) == yrange[2]
  hits_xmin <- min(dat2$x) == xrange[1]
  hits_xmax <- max(dat2$x) == xrange[2]


  if (hits_ymin & !hits_xmin | !hits_xmin & !hits_ymin) {
    dat2 <- dat2 %>% add_row(x = xrange[1], y = yrange[1], bound = bnd, method = "augmented")
  }
  if (hits_ymax & !hits_xmax | !hits_xmax & !hits_ymax) {
    dat2 <- dat2 %>% add_row(x = xrange[2], y = yrange[2], bound = bnd, method = "augmented")
  }
  if (hits_xmax & !hits_ymax) {
    # if the max x value is present, but it doesn't hit the ymax, replace it
    dat2[which(dat2$x == xrange[2] & dat2$y == max(dat2$y)),] <- c(xrange[2], yrange[2], bnd, "augmented", NA)
    dat2 <- mutate(dat2, x = parse_number(x), y = parse_number(y))
  }

  dat2 %>% arrange(x)

}
