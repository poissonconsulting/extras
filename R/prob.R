#' Beta-Binomial Cumulative Distribution Function
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
#' @param x A numeric vector of quantiles.
#'
#' @return An numeric vector of the corresponding probabilities.
#' @family prob_dist
#' @export
#'
#' @examples
#' prob_beta_binom(c(0, 1, 2), 3, 0.5, 0)
prob_beta_binom <- function(x, size = 1, prob = 0.5, theta = 0) {
  rlang::check_installed("extraDistr") # FIXME: this function seems too difficult to implement for now. use extraDistr::pbbinom() for now.
  alpha <- prob * 2 * (1 / theta)
  beta <- (1 - prob) * 2 * (1 / theta)
  extraDistr::pbbinom(q = x, size = size, alpha = alpha, beta = beta)
}


#' Bernoulli Cumulative Distribution Function
#'
#' @inheritParams params
#' @param x A vector of quantiles.
#'
#' @return An numeric vector of the corresponding probabilities.
#' @family prob_dist
#' @export
#'
#' @examples
#' prob_bern(c(TRUE, FALSE), 0.7)
prob_bern <- function(x, prob = 0.5) {
  prob_binom(x, size = 1, prob = prob)
}

#' Binomial Cumulative Distribution Function
#'
#' @inheritParams params
#' @param x A numeric vector of quantiles.
#'
#' @return An numeric vector of the corresponding probabilities.
#' @family prob_dist
#' @export
#'
#' @examples
#' prob_binom(c(0, 1, 2), 2, 0.3)
prob_binom <- function(x, size = 1, prob = 0.5) {
  stats::pbinom(q = x, size = size, prob = prob)
}

#' Gamma Cumulative Distribution Function
#'
#' @inheritParams params
#' @param x A numeric vector of quantiles.
#'
#' @return An numeric vector of the corresponding probabilities.
#' @family prob_dist
#' @export
#'
#' @examples
#' prob_gamma(c(0, 1, 2), 1, 2)
prob_gamma <- function(x, shape = 1, rate = 1) {
  stats::pgamma(q = x, shape = shape, rate = rate)
}

#' Gamma-Poisson Cumulative Distribution Function
#'
#' @inheritParams params
#' @param x A numeric vector of quantiles.
#'
#' @return An numeric vector of the corresponding probabilities.
#' @family prob_dist
#' @export
#'
#' @examples
#' prob_gamma_pois(c(0, 1, 2), 1, 1)
prob_gamma_pois <- function(x, lambda = 1, theta = 0) {
  prob_neg_binom(x, lambda = lambda, theta = theta)
}

#' Zero-Inflated Gamma-Poisson Cumulative Distribution Function
#'
#' @inheritParams params
#' @param x A numeric vector of quantiles.
#'
#' @return An numeric vector of the corresponding probabilities.
#' @family prob_dist
#' @export
#'
#' @examples
#' prob_gamma_pois_zi(c(1, 3, 4), 3, 1, prob = 0.5)
prob_gamma_pois_zi <- function(x, lambda = 1, theta = 0, prob = 0) {
  prob * (x >= 0) + (1 - prob) * stats::pnbinom(q = x, mu = lambda, size = 1 / theta)
}

#' Log-Normal Cumulative Distribution Function
#'
#' @inheritParams params
#' @param x A numeric vector of quantiles.
#'
#' @return An numeric vector of the corresponding probabilities.
#' @family prob_dist
#' @export
#'
#' @examples
#' prob_lnorm(10, 0, 2)
prob_lnorm <- function(x, meanlog = 0, sdlog = 1) {
  stats::plnorm(q = x, meanlog = meanlog, sdlog = sdlog)
}

#' Negative Binomial Cumulative Distribution Function
#'
#' @inheritParams params
#' @param x A numeric vector of quantiles.
#'
#' @return An numeric vector of the corresponding probabilities.
#' @family prob_dist
#' @export
#'
#' @examples
#' prob_neg_binom(c(0, 1, 2), 2, 1)
prob_neg_binom <- function(x, lambda = 1, theta = 0) {
  stats::pnbinom(q = x, mu = lambda, size = 1 / theta)
}

#' Normal Cumulative Distribution Function
#'
#' @inheritParams params
#' @param x A numeric vector of quantiles.
#'
#' @return An numeric vector of the corresponding probabilities.
#' @family prob_dist
#' @export
#'
#' @examples
#' prob_norm(c(-2:2))
prob_norm <- function(x, mean = 0, sd = 1) {
  stats::pnorm(q = x, mean = mean, sd = sd)
}

#' Poisson Cumulative Distribution Function
#'
#' @inheritParams params
#' @param x A numeric vector of quantiles.
#'
#' @return An numeric vector of the corresponding probabilities.
#' @family prob_dist
#' @export
#'
#' @examples
#' prob_pois(c(1, 3, 4), 3)
prob_pois <- function(x, lambda = 1) {
  stats::ppois(q = x, lambda = lambda)
}

#' Zero-Inflated Poisson Cumulative Distribution Function
#'
#' @inheritParams params
#' @param x A numeric vector of quantiles.
#'
#' @return An numeric vector of the corresponding probabilities.
#' @family prob_dist
#' @export
#'
#' @examples
#' prob_pois_zi(c(1, 3, 4), 3, prob = 0.5)
prob_pois_zi <- function(x, lambda = 1, prob = 0) {
  prob * (x >= 0) + (1 - prob) * ppois(q = x, lambda = lambda)
}

#' Skew Normal Cumulative Distribution Function
#'
#' @inheritParams params
#' @param x A numeric vector of quantiles.
#' @param shape A numeric vector of shape.
#'
#' @return An numeric vector of the corresponding probabilities.
#' @family prob_dist
#' @export
#'
#' @examplesIf rlang::is_installed("sn")
#' prob_skewnorm(c(-2:2))
#' prob_skewnorm(c(-2:2), shape = -2)
#' prob_skewnorm(c(-2:2), shape = 2)
prob_skewnorm <- function(x, mean = 0, sd = 1, shape = 0) {
  pskewnorm(q = x, mean = mean, sd = sd, shape = shape)
}

#' Student's t Cumulative Distribution Function
#'
#' @inheritParams params
#' @param x A numeric vector of quantiles.
#'
#' @return An numeric vector of the corresponding probabilities.
#' @family prob_dist
#' @export
#'
#' @examples
#' prob_student(c(1, 3.5, 4), mean = 1, sd = 2, theta = 1 / 3)
prob_student <- function(x, mean = 0, sd = 1, theta = 0) {
  chk::chk_gte(sd, 0) # FIXME: why don't we have more chk functions in the log_lik ones?? ask Joe
  df <- 1 / theta
  stats::pt((x - mean) / sd, df)
}

