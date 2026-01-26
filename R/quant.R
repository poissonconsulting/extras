# FIXME: finish beta-binom
# FIXME: fix examples to make sense with x = probabilities

#' Beta-Binomial Quantile Function
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
#' @param x A numeric vector of probabilities.
#'
#' @return An numeric vector of the corresponding quantiles.
#' @family quant_dist
#' @export
#'
#' @examples
#' quant_beta_binom(c(0.1, 0.4, 0.6), 3, 0.5, 0)
quant_beta_binom <- function(x, size = 1, prob = 0.5, theta = 0) {
  # Needs numerical optimization if theta > 0 - not yet implemented.
  stop("Quantile function for the beta-binomial distribution not currently implemented.")
  alpha <- prob * 2 * (1 / theta)
  beta <- (1 - prob) * 2 * (1 / theta)
  if (!is.na(theta) & theta == 0) {
    return(quant_binom(x = x, size = size, prob = prob))
  }
}


#' Bernoulli Quantile Function
#'
#' @inheritParams params
#' @param x A vector of probabilities.
#'
#' @return An numeric vector of the corresponding quantiles.
#' @family quant_dist
#' @export
#'
#' @examples
#' quant_bern(c(TRUE, FALSE), 0.7)
quant_bern <- function(x, prob = 0.5) {
  qbern(p = x, prob = prob)
}

#' Binomial Quantile Function
#'
#' @inheritParams params
#' @param x A numeric vector of probabilities.
#'
#' @return An numeric vector of the corresponding quantiles.
#' @family quant_dist
#' @export
#'
#' @examples
#' quant_binom(c(0.1, 0.4, 0.6), 2, 0.3)
quant_binom <- function(x, size = 1, prob = 0.5) {
  stats::qbinom(p = x, size = size, prob = prob)
}

#' Gamma Quantile Function
#'
#' @inheritParams params
#' @param x A numeric vector of probabilities.
#'
#' @return An numeric vector of the corresponding quantiles.
#' @family quant_dist
#' @export
#'
#' @examples
#' quant_gamma(c(0.1, 0.4, 0.6), 1, 2)
quant_gamma <- function(x, shape = 1, rate = 1) {
  stats::qgamma(p = x, shape = shape, rate = rate)
}

#' Gamma-Poisson Quantile Function
#'
#' @inheritParams params
#' @param x A numeric vector of probabilities.
#'
#' @return An numeric vector of the corresponding quantiles.
#' @family quant_dist
#' @export
#'
#' @examples
#' quant_gamma_pois(c(0.1, 0.4, 0.6), 1, 1)
quant_gamma_pois <- function(x, lambda = 1, theta = 0) {
  quant_neg_binom(x, lambda = lambda, theta = theta)
}

#' Zero-Inflated Gamma-Poisson Quantile Function
#'
#' @inheritParams params
#' @param x A numeric vector of probabilities.
#'
#' @return An numeric vector of the corresponding quantiles.
#' @family quant_dist
#' @export
#'
#' @examples
#' quant_gamma_pois_zi(c(0.1, 0.4, 0.6), 3, 1, prob = 0.5)
quant_gamma_pois_zi <- function(x, lambda = 1, theta = 0, prob = 0) {
  stats::qnbinom(p = pmax(0, (x - prob) / (1 - prob)), mu = lambda, size = 1 / theta)
}

#' Log-Normal Quantile Function
#'
#' @inheritParams params
#' @param x A numeric vector of probabilities.
#'
#' @return An numeric vector of the corresponding quantiles.
#' @family quant_dist
#' @export
#'
#' @examples
#' quant_lnorm(c(0.1, 0.4, 0.6), 0, 2)
quant_lnorm <- function(x, meanlog = 0, sdlog = 1) {
  stats::qlnorm(p = x, meanlog = meanlog, sdlog = sdlog)
}

#' Negative Binomial Quantile Function
#'
#' @inheritParams params
#' @param x A numeric vector of probabilities.
#'
#' @return An numeric vector of the corresponding quantiles.
#' @family quant_dist
#' @export
#'
#' @examples
#' quant_neg_binom(c(0.1, 0.4, 0.6), 2, 1)
quant_neg_binom <- function(x, lambda = 1, theta = 0) {
  stats::qnbinom(p = x, mu = lambda, size = 1 / theta)
}

#' Normal Quantile Function
#'
#' @inheritParams params
#' @param x A numeric vector of probabilities.
#'
#' @return An numeric vector of the corresponding quantiles.
#' @family quant_dist
#' @export
#'
#' @examples
#' quant_norm(c(0.1, 0.4, 0.6))
quant_norm <- function(x, mean = 0, sd = 1) {
  stats::qnorm(p = x, mean = mean, sd = sd)
}

#' Poisson Quantile Function
#'
#' @inheritParams params
#' @param x A numeric vector of probabilities.
#'
#' @return An numeric vector of the corresponding quantiles.
#' @family quant_dist
#' @export
#'
#' @examples
#' quant_pois(c(0.1, 0.4, 0.6), 3)
quant_pois <- function(x, lambda = 1) {
  stats::qpois(p = x, lambda = lambda)
}

#' Zero-Inflated Poisson Quantile Function
#'
#' @inheritParams params
#' @param x A numeric vector of probabilities.
#'
#' @return An numeric vector of the corresponding quantiles.
#' @family quant_dist
#' @export
#'
#' @examples
#' quant_pois_zi(c(0.1, 0.4, 0.6), 3, prob = 0.5)
quant_pois_zi <- function(x, lambda = 1, prob = 0) {
  stats::qpois(pmax(0, (x - prob) / (1 - prob)), lambda)
}

#' Skew Normal Quantile Function
#'
#' @inheritParams params
#' @param x A numeric vector of probabilities.
#' @param shape A numeric vector of shape.
#'
#' @return An numeric vector of the corresponding quantiles.
#' @family quant_dist
#' @export
#'
#' @examplesIf rlang::is_installed("sn")
#' quant_skewnorm(c(0.1, 0.4, 0.6))
#' quant_skewnorm(c(0.1, 0.4, 0.6), shape = -2)
#' quant_skewnorm(c(0.1, 0.4, 0.6), shape = 2)
quant_skewnorm <- function(x, mean = 0, sd = 1, shape = 0) {
  qskewnorm(p = x, mean = mean, sd = sd, shape = shape)
}

#' Student's t Quantile Function
#'
#' @inheritParams params
#' @param x A numeric vector of probabilities.
#'
#' @return An numeric vector of the corresponding quantiles.
#' @family quant_dist
#' @export
#'
#' @examples
#' quant_student(c(0.1, 0.4, 0.6), mean = 1, sd = 2, theta = 1 / 3)
quant_student <- function(x, mean = 0, sd = 1, theta = 0) {
  chk::chk_number(mean)
  chk::chk_gte(sd)
  df <- 1 / theta
  mean + sd * stats::qt(x, df)
}

