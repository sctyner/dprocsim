#' Simulate from a DP Prior
#'
#' @param M An integer. The precision parameter in a Dirichlet Process Prior.
#' @param F0 Either a function or a non-empty character string naming the function to be called.
#' @param ... Named arguments to pass to to the centering function.
#' @param sticks An integer. The number of sticks to break when simulating from DP(M, F0) using the stick breaking construction.
#'
#' @importFrom dplyr arrange group_by summarize ungroup mutate pull
#' @importFrom stats rbeta runif rnorm
#'
#' @examples
#'
#' beta1 <- dpprior_sim(M = 4, F0 = "rbeta", sticks = 100, shape1 = 4, shape2 = 1)
#' norm1 <- dpprior_sim(M = 1, F0 = rnorm, sticks = 50, mean = 1, sd = .01)
#' @export
dpprior_sim <- function(M, F0, sticks, ...) {
  # stack exchange code
  # https://stats.stackexchange.com/questions/95120/simulate-dirichlet-process-in-r

  b <- rbeta(sticks, 1, M) # sticks
  p <- numeric(sticks)
  p[1] <- b[1]
  p[2:sticks] <- sapply(2:sticks, function(i) b[i] * prod(1 - b[1:(i - 1)])) # weights
  y <- F_0(sticks, F0, ...) # locations

  dpp_samp <- data.frame(weights = p, locations = y)
  dpp_samp %>%
    arrange(locations) %>%
    group_by(locations) %>%
    summarize(weights2 = sum(weights)) %>%
    ungroup() %>%
    mutate(weCDF = cumsum(weights2))%>%
    mutate(weCDF = weCDF / max(weCDF)) # normalize sticks
}



#' Simulate many Distributions from DP Priors
#'
#' @param M Integer. Value of the scaling parameter in the DP prior. May be a vector.
#' @param F0 A character. Function for the centering distribution in the DP prior.
#' @param sticks An integer. The number of sticks to break when simulating from \eqn{DP(M, F_0)} using the stick breaking construction.
#' @param N An integer. The number of distributions to simulate for each value of `M`.
#' @param ... Named arguments to pass to to the centering function.
#' @importFrom tibble as_tibble
#' @importFrom purrr map
#' @importFrom rlang f_text
#'
#' @description Obtain repeated draws from a (set of) given DP priors.
#'
#' @examples
#' M <- 1:2
#' F0 <- "rbeta"
#'
#' dpprior_sim2(M = M, F0 = rbeta, sticks = 100, N = 10, shape1 = 4, shape2 = 1)
#' @export
dpprior_sim2 <- function(M, F0, sticks, N, ...) {
  sim_grid <- expand.grid(rep = 1:N, M = M, stringsAsFactors = FALSE) %>% as_tibble()

  sim_grid %>%
    mutate(draws = map(M,
      dpprior_sim,
      sticks = sticks, F0 = F0, ...
    ))
}

#' Simulate from a DP Posterior
#'
#' @param dat A vector. Observed data.
#' @inheritParams dpprior_sim
#'
#' @examples
#' dat <- runif(25, 0, 1)
#' beta2 <- dppost_sim(dat, M = 4, F0 = "rbeta", sticks = 100, shape1 = 4, shape2 = 1)
#' dat <- runif(25, .75, 1.25)
#' norm2 <- dppost_sim(dat, M = 1, F0 = "rnorm", sticks = 100, mean = 1, sd = .01)
#' @export
dppost_sim <- function(M, F0, sticks, dat, ...) {
  #
  if (!is.vector(dat)) {
    stop("Error: dat must be a vector.")
  }

  n_obs <- length(dat)

  b <- rbeta(sticks, 1, M) # sticks
  p <- numeric(length = sticks)
  p[1] <- b[1]
  p[2:sticks] <- sapply(2:sticks, function(i) b[i] * prod(1 - b[1:(i - 1)])) # weights
  y <- F_0_prime(M = M, F0 = F0, sticks = sticks, dat = dat, ...)

  dpp_samp <- data.frame(weights = p, locations = y)
  dpp_samp %>%
    arrange(locations) %>%
    group_by(locations) %>%
    summarize(weights2 = sum(weights)) %>%
    ungroup() %>%
    mutate(weCDF = cumsum(weights2)) %>%
    mutate(weCDF = weCDF / max(weCDF))
}


#' Simulate many Distributions from DP Posteriors
#' @param N An integer. The number of distributions to simulate for each value of `M`.
#' @param ... Named arguments to pass to to the centering function.
#' @param dat A vector. Observed data.
#' @inheritParams  dpprior_sim
#'
#' @export
dppost_sim2 <- function(M, F0, sticks, dat, N, ...) {
    sim_grid <- expand.grid(rep = 1:N, M = M, stringsAsFactors = FALSE) %>% as_tibble()


  sim_grid %>%
    mutate(draws = map(M,
      dppost_sim,
      sticks = sticks, dat = dat, F0 = F0, ...
    ))
}

#'
#' Simulate from Prior Centering Measure
#'
#' @param F0 A character. Name of the function for simulating from the desired centering base measure.
#' @param sticks An integer. The number of sticks to break when simulating from \eqn{DP(M, F_0)} using the stick breaking construction.
#' @param ... Named arguments to pass to to the centering function.

#'
#' @description Prior form of the centering measure.
#' @export
F_0 <- function(sticks, F0, ...) {
  do.call(
    what = F0,
    args = list(n = sticks, ...)
  )
}
#'
#' Simulate from Posterior Centering Measure
#'
#' @param M An integer. The precision parameter in a Dirichlet Process Prior.
#' @param F0 A character. Name of the function for simulating from the desired centering base measure.
#' @param sticks An integer. The number of sticks to break when simulating from \eqn{DP(M, F_0)} using the stick breaking construction.
#' @param ... Named arguments to pass to to the centering function.
#' @param dat A vector. Observed data.
#'
#' @description Posterior (conjugate) form of the centering measure.
#' @export
F_0_prime <- function(M, F0, sticks, dat, ...) {
  n_obs <- length(dat)
  locations <- numeric(sticks)

  # ideally there would not be a for loop here but I'm not sure how to
  # modify rn.
  for (i in 1:sticks) {
    x <- runif(1)
    if (x < (M / (M + n_obs))) { # with prob M/(M+n)
      locations[i] <- do.call(F0, list(n = 1, ...)) # sample 1 location from F_0
    } else { # with prob n/(M+n),
      locations[i] <- sample(dat, 1) # sample 1 location from the data
    }
  }
  locations
}
