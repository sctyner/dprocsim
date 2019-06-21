#' Check CDF values fall in between bounds
#'
#' @param dpp_draw Data frame. A single draw of a CDF from a Dirichlet Process. Output of the `dp*_sim*()` functions. Should have columns names `locations` and `weCDF`.
#' @param ubf Function. The upper bound function created with the clicked data points. Output of either `spline_funs()` or `linear_funs()`.
#' @param lbf Function. The lower bound function created with the clicked data points. Output of either `spline_funs()` or `linear_funs()`.
#' @param x Numeric. A single x-value to check in the domain of the CDF.
#'
#' @export
check_cdf_bounds <- function(dpp_draw, ubf, lbf, x){
  F0_up <- do.call(ubf, list(x))
  F0_low <- do.call(lbf, list(x))
  # get the max x value in the drawn cdf that doesn't go over the xgrid value
  draw_dat <- filter(dpp_draw, locations < x) %>% filter(locations == max(locations))

  if (nrow(draw_dat) == 0){
    F0_val <- 0
  } else {
    F0_val <- draw_dat %>% pull(weCDF)
  }

  if (F0_val > F0_up){
    "above"
  } else if (F0_val < F0_low){
    "below"
  } else {
    "between"
  }

}
#'
#' Check CDF over a grid of x values
#'
#' @inheritParams check_cdf_bounds
#' @param xgrid Numeric. A vector of x values in the domain of the CDF to check. It is recommended you don't put the bounds in.
#'
#' @importFrom purrr map
#'
#' @describeIn check_cdf_bounds
#'
#' @export
check_whole_cdf <- function(dpp_draw, ubf, lbf, xgrid){
  checks <- unlist(map(xgrid, check_cdf_bounds, dpp_draw = dpp_draw, ubf = ubf, lbf = lbf))
  tibble(x = xgrid, region = checks)
}
#'
#' Check many CDFs over a grid of x values
#'
#' @param dpp_draws A tibble. The output from `dpprior_sim2()` (or `dppost_sim2()`).
#' @inheritParams  check_whole_cdf
#'
#' @describeIn check_cdf_bounds
#'
#' @export
check_cdf_many <- function(dpp_draws, ubf, lbf, xgrid){
  dpp_draws %>%
    mutate(regions = map(draws, check_whole_cdf,
                         ubf = ubf, lbf = lbf, xgrid = xgrid))
}
#' Determine regions of random CDFs.
#'
#' @param many_draws_checked A tibble. The output from `check_cdf_many()`
#' @param N An integer. How many repeated draws are in `many_draws_checked`? Same `N` as that of `dp*_sim2()`.
#' @importFrom dplyr select group_by count mutate_at vars
#' @importFrom tidyr unnest spread
#'
#' @describeIn check_cdf_bounds
#'
#' @export
check_cdf_counts <- function(many_draws_checked, N) {
  many_draws_checked %>% select(-draws) %>% unnest() %>%
    group_by(M, x) %>%
    count(region) %>%
    spread(region, n, fill = 0) %>%
    mutate_at(vars("above", "below", "between"), function(x) x / N)
}
#'
#' Check that the given proportions of CDFs fall in the given regions
#'
#' @param cdf_regions A tibble. Output from `check_cdf_counts()`
#' @param au Numeric. A number between 0-1. What proportion of simulated CDFs should lie above the upper bound?
#' @param bl Numeric. A number between 0-1. What proportion of simulated CDFs should lie below the lower bound?
#' @param bw Numeric. A number between 0-1. What proportion of simulated CDFs should between the upper and lower bounds?
#'
#' @importFrom purrr pmap_lgl
#'
check_cdf_regions <- function(cdf_regions, au = .1, bl = .1, bw = .5){
  cdf_regions %>%
  mutate(check3 = pmap_lgl(list(above, below, between),
                           function(a,b,c){
                             a > au & b > bl & c > bw
                           })) %>%
    group_by(M) %>% count(check3)
}


