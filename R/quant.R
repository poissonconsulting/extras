
#' Beta Quantile Function
#'
#' @inheritParams params
#' @param x A numeric vector of probabilities.
#'
#' @return An numeric vector of the corresponding quantiles.
#' @family quant_dist
#' @export
#'
#' @examples
#' quant_beta(c(0.1, 0.4, 0.6), 2, 3)
quant_beta <- function(x, alpha = 1, beta = 1) {
  stats::qbeta(p = x, shape1 = alpha, shape2 = beta)
}

#' Bernoulli Quantile Function
#'
#' @inheritParams params
#' @param x A numeric vector of probabilities.
#'
#' @return An numeric vector of the corresponding quantiles.
#' @family quant_dist
#' @export
#'
#' @examples
#' quant_bern(c(0.3, 0.8), 0.7)
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

#' Skew-Lognormal Quantile Function
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
#' quant_skewlnorm(c(0.1, 0.4, 0.6))
#' quant_skewlnorm(c(0.1, 0.4, 0.6), shape = -2)
#' quant_skewlnorm(c(0.1, 0.4, 0.6), shape = 2)
quant_skewlnorm <- function(x, meanlog = 0, sdlog = 1, shape = 0) {
  qskewlnorm(p = x, meanlog = meanlog, sdlog = sdlog, shape = shape)
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
  chk::chk_gte(sd, 0)
  df <- 1 / theta
  mean + sd * stats::qt(x, df)
}


#' Exponential Quantile Function
#'
#' @inheritParams params
#' @param x A numeric vector of probabilities.
#'
#' @return An numeric vector of the corresponding quantiles.
#' @family quant_dist
#' @export
#'
#' @examples
#' quant_exp(c(0.1, 0.4, 0.6), 2)
quant_exp <- function(x, rate = 1) {
  stats::qexp(p = x, rate = rate)
}

#' Uniform Quantile Function
#'
#' @inheritParams params
#' @param x A numeric vector of probabilities.
#'
#' @return An numeric vector of the corresponding quantiles.
#' @family quant_dist
#' @export
#'
#' @examples
#' quant_unif(c(0.1, 0.4, 0.6))
quant_unif <- function(x, min = 0, max = 1) {
  stats::qunif(p = x, min = min, max = max)
}
