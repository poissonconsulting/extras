#' Beta-Binomial Random Samples
#'
#' This parameterization of the beta-binomial distribution uses an expected
#' probability parameter, `prob`, and a dispersion parameter, `theta`. The
#' parameters of the underlying beta mixture are `alpha = (2 * prob) / theta`
#' and `beta = (2 * (1 - prob)) / theta`. This parameterization of `theta` is
#' unconventional, but has useful properties when modelling. When `theta = 0`,
#' the beta-binomial reverts to the binomial distribution. When `theta = 1` and
#' `prob = 0.5`, the parameters of the beta distribution become `alpha = 1` and
#' `beta = 1`, which correspond to a uniform distribution for the beta-binomial
#' probability parameter.
#'
#' @inheritParams params
#' @return A numeric vector of the random samples.
#' @family ran_dist
#' @export
#'
#' @examples
#' ran_beta_binom(10, 1, 0.5, 0)
ran_beta_binom <- function(n = 1, size = 1, prob = 0.5, theta = 0) {
  chk_whole_number(n)
  chk_gte(n)
  alpha <- prob * 2 * (1 / theta)
  beta <- (1 - prob) * 2 * (1 / theta)
  p <- stats::rbeta(n, shape1 = alpha, shape2 = beta)
  use_binom <- !is.na(theta) & theta == 0
  p[use_binom] <- prob
  stats::rbinom(n, size = size, prob = p)
}

#' Bernoulli Random Samples
#'
#' @inheritParams params
#' @return A numeric vector of the random samples.
#' @family ran_dist
#' @export
#'
#' @examples
#' ran_bern(10)
ran_bern <- function(n = 1, prob = 0.5) {
  ran_binom(n, size = 1, prob = prob)
}

#' Binomial Random Samples
#'
#' @inheritParams params
#' @return A numeric vector of the random samples.
#' @family ran_dist
#' @export
#'
#' @examples
#' ran_binom(10)
ran_binom <- function(n = 1, size = 1, prob = 0.5) {
  chk_whole_number(n)
  chk_gte(n)
  stats::rbinom(n, size = size, prob = prob)
}

#' Gamma Random Samples
#'
#' @inheritParams params
#' @return A numeric vector of the random samples.
#' @family ran_dist
#' @export
#'
#' @examples
#' ran_gamma(10)
ran_gamma <- function(n = 1, shape = 1, rate = 1) {
  chk_whole_number(n)
  chk_gte(n)
  stats::rgamma(n, shape = shape, rate = rate)
}

#' Gamma-Poisson Random Samples
#'
#' @inheritParams params
#' @return A numeric vector of the random samples.
#' @family ran_dist
#' @export
#'
#' @examples
#' ran_gamma_pois(10, theta = 1)
ran_gamma_pois <- function(n = 1, lambda = 1, theta = 0) {
  ran_neg_binom(n = n, lambda = lambda, theta = theta)
}

#' Zero-Inflated Gamma-Poisson Random Samples
#'
#' @inheritParams params
#' @return A numeric vector of the random samples.
#' @family ran_dist
#' @export
#'
#' @examples
#' ran_gamma_pois_zi(10, lambda = 3, theta = 1, prob = 0.5)
ran_gamma_pois_zi <- function(n = 1, lambda = 1, theta = 0, prob = 0) {
  ran_neg_binom(n = n, lambda = lambda, theta = theta) * ran_bern(n, prob = 1 - prob)
}

#' Log-Normal Random Samples
#'
#' @inheritParams params
#' @return A numeric vector of the random samples.
#' @family ran_dist
#' @export
#'
#' @examples
#' ran_lnorm(10)
ran_lnorm <- function(n = 1, meanlog = 0, sdlog = 1) {
  chk_whole_number(n)
  chk_gte(n)
  stats::rlnorm(n, meanlog = meanlog, sdlog = sdlog)
}

#' Negative Binomial Random Samples
#'
#' Identical to Gamma-Poisson Random Samples.
#'
#' @inheritParams params
#' @return A numeric vector of the random samples.
#' @family ran_dist
#' @export
#'
#' @examples
#' ran_neg_binom(10, theta = 1)
ran_neg_binom <- function(n = 1, lambda = 1, theta = 0) {
  chk_whole_number(n)
  chk_gte(n)
  as.integer(stats::rnbinom(n = n, mu = lambda, size = 1 / theta))
}

#' Normal Random Samples
#'
#' @inheritParams params
#' @return A numeric vector of the random samples.
#' @family ran_dist
#' @export
#'
#' @examples
#' ran_norm(10)
ran_norm <- function(n = 1, mean = 0, sd = 1) {
  chk_whole_number(n)
  chk_gte(n)
  stats::rnorm(n, mean = mean, sd = sd)
}

#' Poisson Random Samples
#'
#' @inheritParams params
#' @return A numeric vector of the random samples.
#' @family ran_dist
#' @export
#'
#' @examples
#' ran_pois(10)
ran_pois <- function(n = 1, lambda = 1) {
  chk_whole_number(n)
  chk_gte(n)
  stats::rpois(n, lambda = lambda)
}

#' Zero-Inflated Poisson Random Samples
#'
#' @inheritParams params
#' @return A numeric vector of the random samples.
#' @family ran_dist
#' @export
#'
#' @examples
#' ran_pois_zi(10, prob = 0.5)
ran_pois_zi <- function(n = 1, lambda = 1, prob = 0) {
  stats::rpois(n, lambda = lambda) * ran_bern(n, prob = 1 - prob)
}

#' Skew Normal Random Samples
#'
#' @inheritParams params
#' @param shape A numeric vector of shape.
#' @return A numeric vector of the random samples.
#' @family ran_dist
#' @export
#'
#' @examples
#' ran_skewnorm(10, shape = -1)
#' ran_skewnorm(10, shape = 0)
#' ran_skewnorm(10, shape = 1)
ran_skewnorm <- function(n = 1, mean = 0, sd = 1, shape = 0) {
  chk_whole_number(n)
  chk_gte(n)
  rskewnorm(n = n, mean = mean, sd = sd, shape = shape)
}

#' Student's t Random Samples
#'
#' @inheritParams params
#' @return A numeric vector of the random samples.
#' @family ran_dist
#' @export
#'
#' @examples
#' ran_student(10, theta = 1 / 2)
ran_student <- function(n = 1, mean = 0, sd = 1, theta = 0) {
  chk_whole_number(n)
  if (length(mean) > n) {
    mean <- mean[1:n]
  }
  if (length(sd) > n) {
    sd <- sd[1:n]
  }
  df <- 1 / theta
  x <- stats::rt(n, df)
  r <- x * sd + mean
  r
}
