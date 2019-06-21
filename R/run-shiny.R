#' Run Built-In Shiny App(s)
#'
#' @param appname Character. Name of the app to run. There is only one option: `"cdf-picker"`
#'
#' @description This app allows you to draw upper and lowers bounds for a CDF. When simulating from Dirichlet Process Priors with centering distribution as the mean of the upper and lower bounds drawns, we expect that some CDFs simulated will fall outside of the given bounds. Be thinking about what proportion of simulated CDFs you want to fall outside of
#'
#' @examples
#' \dontrun{
#' run_shiny()
#' }
#'
#' @export
run_shiny <- function(appname = "cdf-picker") {
  appDir <- system.file("shiny", appname, package = "dprocsim")
  if (appDir == "") {
    stop("Could not find shiny directory. Try re-installing `dprocsim`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
