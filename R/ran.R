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
ran_beta_binom <- function(n = 1, size = 1, prob = 0.5, theta = 0, tlower = 0, tupper = Inf) {
  chk_whole_number(n)
  chk_gte(n)
  alpha <- prob * 2 * (1 / theta)
  beta <- (1 - prob) * 2 * (1 / theta)
  p <- stats::rbeta(n, shape1 = alpha, shape2 = beta)
  use_binom <- !is.na(theta) & theta == 0
  p[use_binom] <- prob
  # stats::rbinom(n, size = size, prob = p)
  # FIXME: maybe only do this version if tlower or tupper are different from their defaults??
  ran <- ran_binom(n = n, size = size, prob = p, tlower = tlower, tupper = tupper)
  ran[1:n]
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
  # Again, doesn't make sense to have a truncated bernoulli distribution??
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
ran_binom <- function(n = 1, size = 1, prob = 0.5, tlower = 0, tupper = Inf) {
  chk_whole_number(n)
  chk_gte(n)

  # stats::rbinom(n, size = size, prob = prob)
  p_lower <- prob_binom(tlower, size = size, prob = prob)
  p_upper <- prob_binom(tupper, size = size, prob = prob)
  u <- runif(n, p_lower, p_upper)
  ran <- as.integer(quant_binom(u, size = size, prob = prob))
  ran[1:n]
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
ran_gamma <- function(n = 1, shape = 1, rate = 1, tlower = 0, tupper = Inf) {
  chk_whole_number(n)
  chk_gte(n)
  # stats::rgamma(n, shape = shape, rate = rate)
  p_lower <- prob_gamma(tlower, shape = shape, rate = rate)
  p_upper <- prob_gamma(tupper, shape = shape, rate = rate)
  u <- runif(n, p_lower, p_upper)
  ran <- quant_gamma(u, shape = shape, rate = rate)
  ran[1:n]
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
ran_gamma_pois <- function(n = 1, lambda = 1, theta = 0, tlower = 0, tupper = Inf) {
  ran_neg_binom(n = n, lambda = lambda, theta = theta, tlower = tlower, tupper = tupper)
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
ran_gamma_pois_zi <- function(n = 1, lambda = 1, theta = 0, prob = 0, tlower = 0, tupper = Inf) {
  if (tlower > 0 & !is.na(tlower)) {
    # FIXME: what is the correct function to use to issue a warning
    warning("Specifying a lower truncation point greater than 0 doesn't make sense for a zero-inflated distribution.")
  }
  ran_neg_binom(n = n, lambda = lambda, theta = theta, tlower = tlower, tupper = tupper) * ran_bern(n, prob = 1 - prob)
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
ran_lnorm <- function(n = 1, meanlog = 0, sdlog = 1, tlower = 0, tupper = Inf) {
  chk_whole_number(n)
  chk_gte(n)
  # stats::rlnorm(n, meanlog = meanlog, sdlog = sdlog)
  p_lower <- prob_lnorm(tlower, meanlog = meanlog, sdlog = sdlog)
  p_upper <- prob_lnorm(tupper, meanlog = meanlog, sdlog = sdlog)
  u <- runif(n, p_lower, p_upper)
  ran <- quant_lnorm(u, meanlog = meanlog, sdlog = sdlog)
  ran[1:n]
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
ran_neg_binom <- function(n = 1, lambda = 1, theta = 0, tlower = 0, tupper = Inf) {
  chk_whole_number(n)
  chk_gte(n)
  # as.integer(stats::rnbinom(n = n, mu = lambda, size = 1 / theta))
  p_lower <- prob_neg_binom(tlower, lambda = lambda, theta = theta)
  p_upper <- prob_neg_binom(tupper, lambda = lambda, theta = theta)
  u <- runif(n, p_lower, p_upper)
  ran <- as.integer(quant_neg_binom(u, lambda = lambda, theta = theta))
  ran[1:n]
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
ran_norm <- function(n = 1, mean = 0, sd = 1, tlower = -Inf, tupper = Inf) {
  chk_whole_number(n)
  chk_gte(n)
  # stats::rnorm(n, mean = mean, sd = sd)
  p_lower <- prob_norm(tlower, mean = mean, sd = sd)
  p_upper <- prob_norm(tupper, mean = mean, sd = sd)
  u <- runif(n, p_lower, p_upper)
  ran <- quant_norm(u, mean = mean, sd = sd)
  ran[1:n]
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
ran_pois <- function(n = 1, lambda = 1, tlower = 0, tupper = Inf) {
  chk_whole_number(n)
  chk_gte(n)
  # stats::rpois(n, lambda = lambda)
  p_lower <- prob_pois(tlower, lambda = lambda)
  p_upper <- prob_pois(tupper, lambda = lambda)
  u <- runif(n, p_lower, p_upper)
  ran <- as.integer(quant_pois(u, lambda = lambda))
  ran[1:n]
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
ran_pois_zi <- function(n = 1, lambda = 1, prob = 0, tlower = 0, tupper = Inf) {
  if (tlower > 0 & !is.na(tlower)) {
    # FIXME: what is the correct function to use to issue a warning
    warning("Specifying a lower truncation point greater than 0 doesn't make sense for a zero-inflated distribution.")
  }
  # stats::rpois(n, lambda = lambda) * ran_bern(n, prob = 1 - prob)
  ran <- ran_pois(n, lambda = lambda, tlower = tlower, tupper = Inf) * ran_bern(n, prob = 1 - prob)
  ran[1:n]
}

#' Skew Normal Random Samples
#'
#' @inheritParams params
#' @param shape A numeric vector of shape.
#' @return A numeric vector of the random samples.
#' @family ran_dist
#' @export
#'
#' @examplesIf rlang::is_installed("sn")
#' ran_skewnorm(10, shape = -1)
#' ran_skewnorm(10, shape = 0)
#' ran_skewnorm(10, shape = 1)
ran_skewnorm <- function(n = 1, mean = 0, sd = 1, shape = 0, tlower = -Inf, tupper = Inf) {
  rlang::check_installed("sn")
  chk_whole_number(n)
  chk_gte(n)
  # rskewnorm(n = n, mean = mean, sd = sd, shape = shape)
  p_lower <- prob_skewnorm(tlower, mean = mean, sd = sd, shape = shape)
  p_upper <- prob_skewnorm(tupper, mean = mean, sd = sd, shape = shape)
  u <- runif(n, p_lower, p_upper)
  ran <- quant_skewnorm(u, mean = mean, sd = sd, shape = shape)
  ran[1:n]
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
ran_student <- function(n = 1, mean = 0, sd = 1, theta = 0, tlower = -Inf, tuuper = Inf) {
  chk_whole_number(n)
  # FIXME: consider doing this for the above functions!
  if (length(mean) > n) {
    mean <- mean[1:n]
  }
  if (length(sd) > n) {
    sd <- sd[1:n]
  }
  # df <- 1 / theta
  # x <- stats::rt(n, df)
  # r <- x * sd + mean
  # r
  p_lower <- prob_student(tlower, mean = mean, sd = sd, theta = theta)
  p_upper <- prob_student(tupper, mean = mean, sd = sd, theta = theta)
  u <- runif(n, p_lower, p_upper)
  ran <- quant_student(u, mean = mean, sd = sd, theta = theta)
  ran[1:n]
}
