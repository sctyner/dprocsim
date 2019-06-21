#' Simulate from a drawn CDF
#'
#' @param shinydat Data frame. Output from the shiny app containing information on the two sets of clicked
#' bounds, the two smoothed functions, and the resulting mean function.
#' @param xgrid Vector. Sequence of domain values at which to evaluate the CDF.
#' @param ubnd Character. Name of upper bound. Either `"bound 1"` or `"bound 2"`.
#' @param lbnd Character. Name of lower bound. Either `"bound 1"` or `"bound 2"`.
#' @param meth Character. Approximation method. either `"splines"` for `smooth.spline()` or `"linear"` for `approxfun()`
#'
#' @description After using the shiny app, `dprocsim::run_shiny()`, take the downloaded data and simulate from the resulting mean CDF value.
#'
#' @importFrom readr parse_number
#' @importFrom stats approxfun smooth.spline predict
#'
#'
#' @export
drawn_cdf_sim <- function(shinydat, xgrid, ubnd = "bound 1", lbnd = "bound 2", meth){
   # check both bounds are present
  check1 <- match(c("bound 1", "bound 2"), unique(shinydat$bound))
  if (any(is.na(check1))){
    msg <- paste0("Error: ", c("bound 1", "bound 2")[is.na(check1)], " missing. Return to shiny app to create, smooth, and save both bounds.")
    stop(msg)
  }

  # get the upper and lower bounds
  upper <- shinydat %>% filter(bound == ubnd, method == "clicked") %>% augment_cdf(xrange = range(xgrid))
  lower <- shinydat %>% filter(bound == lbnd, method == "clicked") %>% augment_cdf(xrange = range(xgrid))

  # mean of upper and lower on the grid
  F0_values <- mean_funs(upper = upper, lower = lower, meth = meth)(xgrid)


  function(n){
    # draw random uniforms
      new_ys <- runif(n)
    # like the PIT.
    # random draw from F_0 = approxfun(x = CDF values, y = grid of Xs)(random uniforms)
    draw_F0 <- approxfun(x = F0_values, y = xgrid)(new_ys)

    draw_df <- data.frame(new.ys = new_ys, new.xs = draw_F0) %>% arrange(new.ys)

    if (nrow(filter(draw_df, is.na(new.xs))) > 0){
      if(which.min(draw_df$new.xs) > 1){
        draw_df$new.xs[1:(which.min(draw_df$new.xs)-1)] <- min(xgrid)
      }
      if (which.max(draw_df$new.xs) < nrow(draw_df)){
        draw_df$new.xs[(which.max(draw_df$new.xs)+1):(nrow(draw_df))] <- max(xgrid)
      }
    }

    draw_df %>% pull(new.xs) %>% sample()
  }

}

#' Helpful closures
#'
#' @param clicks Clicked data
#' @param xrange Vector of length 2 giving the minimum and maximum domain values for the spline function.
#' @param spar Numeric. Value of the smoothing parameter (between 0-1) to be passed to `smooth.spline()`.
#'
#' @description Create linear approximation or spline approximation functions from a given set of points.
#'
#' @examples
#' \dontrun{
#' splinefuns(clicks)(seq(xmin, xmax, length.out = 100))
#' }
#'
#' @export
spline_funs <- function(clicks, xrange, spar = .5){
  clicks <- augment_cdf(clicks, xrange = xrange)
  spline1 <- smooth.spline(x = clicks$x, y = clicks$y, spar = spar)
  function(xgrid){
    new <- predict(spline1, xgrid)
    new$y[new$x == xrange[2]] <- 1
    new$y[new$y > 1] <- 1
    new$y[new$x == xrange[1]] <- 0
    new$y[new$y < 0] <- 0
    new$y
  }
}
#' Linear closure
#'
#' @inheritParams spline_funs
#' @describeIn spline_funs Linear function closure
#' @export
linear_funs <- function(clicks, xrange){
  clicks <- augment_cdf(clicks, xrange = xrange)
  linear1 <- approxfun(x = clicks$x, y= clicks$y)
  function(xgrid){
    preds <- linear1(xgrid)
    if (any(is.na(preds))){
      if(which.min(preds) >1){
        preds[1:(which.min(preds)-1)] <- 0
      }
      if (which.max(preds) < length(preds)){
        preds[(which.max(preds)+1):(length(preds))] <- 1
      }
    }
    preds
  }
}
#' Mean closure
#'
#' @param upper Data frame. The upper bound of points for the cdf
#' @param lower Data frame. The lower bound of points for the cdf
#' @param meth Character. Approximation method. either `"splines"` for `smooth.spline()` or `"linear"` for `approxfun()`
#'
#' @describeIn spline_funs Mean function closure
#' @export
mean_funs <- function(upper, lower, meth){
    function(xgrid){
      if (meth == "splines"){
      (spline_funs(clicks = upper, xrange = range(xgrid))(xgrid)  + spline_funs(clicks = lower, xrange = range(xgrid))(xgrid))/2
      } else {
      (linear_funs(clicks = upper,xrange = range(xgrid))(xgrid)  + linear_funs(clicks = lower,xrange = range(xgrid))(xgrid))/2
    }
  }
}

