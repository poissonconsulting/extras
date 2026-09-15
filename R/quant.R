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
  stats::qnbinom(
    p = pmax(0, (x - prob) / (1 - prob)),
    mu = lambda,
    size = 1 / theta
  )
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
#' @inheritParams dskewnorm
#' @param x A numeric vector of probabilities.
#'
#' @return An numeric vector of the corresponding quantiles.
#' @family quant_dist
#' @export
#'
#' @examplesIf rlang::is_installed("sn")
#' quant_skewnorm(c(0.1, 0.4, 0.6))
#' quant_skewnorm(c(0.1, 0.4, 0.6), shape = -2)
#' quant_skewnorm(c(0.1, 0.4, 0.6), shape = 2)
quant_skewnorm <- function(x, location = 0, scale = 1, shape = 0, ..., mean, sd) {
  if (!missing(mean)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "quant_skewnorm(mean)",
                              id = "quant_skewnorm location",
                              with = "quant_skewnorm(location)")
    location <- mean
  }
  if (!missing(sd)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "quant_skewnorm(sd)",
                              id = "quant_skewnorm scale",
                              with = "quant_skewnorm(scale)")
    scale <- sd
  }
  chk_unused(...)
  qskewnorm(p = x, location = location, scale = scale, shape = shape)
}

#' Skew-Lognormal Quantile Function
#'
#' @inheritParams dskewlnorm
#' @param x A numeric vector of probabilities.
#'
#' @return An numeric vector of the corresponding quantiles.
#' @family quant_dist
#' @export
#'
#' @examplesIf rlang::is_installed("sn")
#' quant_skewlnorm(c(0.1, 0.4, 0.6))
#' quant_skewlnorm(c(0.1, 0.4, 0.6), shape_log = -2)
#' quant_skewlnorm(c(0.1, 0.4, 0.6), shape_log = 2)
quant_skewlnorm <- function(x, location_log = 0, scale_log = 1, shape_log = 0,
                            ..., meanlog, sdlog, shape) {
  if (!missing(meanlog)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "quant_skewlnorm(meanlog)",
                              id = "quant_skewlnorm location_log",
                              with = "quant_skewlnorm(location_log)")
    location_log <- meanlog
  }
  if (!missing(sdlog)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "quant_skewlnorm(sdlog)",
                              id = "quant_skewlnorm scale_log",
                              with = "quant_skewlnorm(scale_log)")
    scale_log <- sdlog
  }
  if (!missing(shape)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "quant_skewlnorm(shape)",
                              id = "quant_skewlnorm shape_log",
                              with = "quant_skewlnorm(shape_log)")
    shape_log <- shape
  }
  chk_unused(...)
  qskewlnorm(p = x, location_log = location_log, scale_log = scale_log,
             shape_log = shape_log)
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
