#' Beta-Binomial Log-Likelihood
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
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding log-likelihoods.
#' @family log_lik_dist
#' @export
#'
#' @examples
#' log_lik_beta_binom(c(0, 1, 2), 3, 0.5, 0)
log_lik_beta_binom <- function(x, size = 1, prob = 0.5, theta = 0) {
  alpha <- prob * 2 * (1 / theta)
  beta <- (1 - prob) * 2 * (1 / theta)
  lbeta_binom <- lgamma(size + 1) - lgamma(x + 1) - lgamma(size - x + 1) +
    lgamma(x + alpha) + lgamma(size - x + beta) - lgamma(size + alpha + beta) +
    lgamma(alpha + beta) - lgamma(alpha) - lgamma(beta)
  bol <- !is.na(x + size + prob + theta)
  lbeta_binom[bol & ((x == 0 & prob == 0) | (x == size & prob == 1))] <- 0
  lbeta_binom[bol & x != 0 & prob == 0] <- -Inf
  lbeta_binom[bol & x != size & prob == 1] <- -Inf
  lbeta_binom[bol & x > size] <- -Inf
  bol_theta <- !is.na(theta)
  lbeta_binom[bol_theta & theta < 0] <- NaN
  use_binom <- bol_theta & theta == 0
  if (any(use_binom)) {
    lbinom <- log_lik_binom(x, size = size, prob = prob)
    lbeta_binom[use_binom] <- lbinom[use_binom]
  }
  if (length(bol) == 0) {
    lbeta_binom <- numeric(0)
  }
  lbeta_binom
}

#' Bernoulli Log-Likelihood
#'
#' @inheritParams params
#' @param x A vector of 0s and 1s.
#'
#' @return An numeric vector of the corresponding log-likelihoods.
#' @family log_lik_dist
#' @export
#'
#' @examples
#' log_lik_bern(c(TRUE, FALSE), 0.7)
log_lik_bern <- function(x, prob = 0.5) {
  log_lik_binom(x, size = 1, prob = prob)
}

#' Binomial Log-Likelihood
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding log-likelihoods.
#' @family log_lik_dist
#' @export
#'
#' @examples
#' log_lik_binom(c(0, 1, 2), 2, 0.3)
log_lik_binom <- function(x, size = 1, prob = 0.5) {
  dbinom(x, size = size, prob = prob, log = TRUE)
}

#' Gamma Log-Likelihood
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#'
#' @return An numeric vector of the corresponding log-likelihoods.
#' @family log_lik_dist
#' @export
#'
#' @examples
#' log_lik_gamma(c(0, 1, 2), 1, 2)
log_lik_gamma <- function(x, shape = 1, rate = 1) {
  stats::dgamma(x, shape = shape, rate = rate, log = TRUE)
}

#' Gamma-Poisson Log-Likelihood
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding log-likelihoods.
#' @family log_lik_dist
#' @export
#'
#' @examples
#' log_lik_gamma_pois(c(0, 1, 2), 1, 1)
log_lik_gamma_pois <- function(x, lambda = 1, theta = 0) {
  log_lik_neg_binom(x, lambda = lambda, theta = theta)
}

#' Zero-Inflated Gamma-Poisson Log-Likelihood
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding log-likelihoods.
#' @family log_lik_dist
#' @export
#'
#' @examples
#' log_lik_gamma_pois_zi(c(1, 3, 4), 3, 1, prob = 0.5)
log_lik_gamma_pois_zi <- function(x, lambda = 1, theta = 0, prob = 0) {
  lpois <- dnbinom(x, mu = lambda, size = 1 / theta)
  lpois <- lpois * (1 - prob)
  zero <- !is.na(x) & x == 0
  if (length(prob) == 1) {
    prob <- rep(prob, length(lpois))
  }
  lpois[zero] <- lpois[zero] + prob[zero]
  log(lpois)
}

#' Log-Normal Log-Likelihood
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#'
#' @return An numeric vector of the corresponding log-likelihoods.
#' @family log_lik_dist
#' @export
#'
#' @examples
#' log_lik_lnorm(10, 0, 2)
log_lik_lnorm <- function(x, meanlog = 0, sdlog = 1) {
  dlnorm(x, meanlog = meanlog, sdlog = sdlog, log = TRUE)
}

#' Negative Binomial Log-Likelihood
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding log-likelihoods.
#' @family log_lik_dist
#' @export
#'
#' @examples
#' log_lik_neg_binom(c(0, 1, 2), 2, 1)
log_lik_neg_binom <- function(x, lambda = 1, theta = 0) {
  dnbinom(x, mu = lambda, size = 1 / theta, log = TRUE)
}

#' Normal Log-Likelihood
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#'
#' @return An numeric vector of the corresponding log-likelihoods.
#' @family log_lik_dist
#' @export
#'
#' @examples
#' log_lik_norm(c(-2:2))
log_lik_norm <- function(x, mean = 0, sd = 1) {
  dnorm(x, mean = mean, sd = sd, log = TRUE)
}

#' Poisson Log-Likelihood
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding log-likelihoods.
#' @family log_lik_dist
#' @export
#'
#' @examples
#' log_lik_pois(c(1, 3, 4), 3)
log_lik_pois <- function(x, lambda = 1) {
  dpois(x, lambda, log = TRUE)
}

#' Zero-Inflated Poisson Log-Likelihood
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding log-likelihoods.
#' @family log_lik_dist
#' @export
#'
#' @examples
#' log_lik_pois_zi(c(1, 3, 4), 3, prob = 0.5)
log_lik_pois_zi <- function(x, lambda = 1, prob = 0) {
  lpois <- dpois(x, lambda = lambda)
  lpois <- lpois * (1 - prob)
  zero <- x == 0
  lpois[zero] <- lpois[zero] + prob
  log(lpois)
}

#' Skew Normal Log-Likelihood
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#' @param shape A numeric vector of shape.
#'
#' @return An numeric vector of the corresponding log-likelihoods.
#' @family log_lik_dist
#' @export
#'
#' @examples
#' log_lik_skewnorm(c(-2:2))
#' log_lik_skewnorm(c(-2:2), shape = -2)
#' log_lik_skewnorm(c(-2:2), shape = 2)
log_lik_skewnorm <- function(x, mean = 0, sd = 1, shape = 0) {
  log_lik <- dskewnorm(x = x, mean = mean, sd = sd, shape = shape, log = TRUE)
  use_norm <- !is.na(shape) & shape == 0
  lnorm <- log_lik_norm(x = x, mean = mean, sd = sd)
  lengths <- as.logical(length(x)) + as.logical(length(mean)) + as.logical(length(sd)) + as.logical(length(shape))
  if (lengths >= 4) log_lik[use_norm] <- lnorm[use_norm]
  log_lik
}

#' Student's t Log-Likelihood
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#'
#' @return An numeric vector of the corresponding log-likelihoods.
#' @family log_lik_dist
#' @export
#'
#' @examples
#' log_lik_student(c(1, 3.5, 4), mean = 1, sd = 2, theta = 1 / 3)
log_lik_student <- function(x, mean = 0, sd = 1, theta = 0) {
  df <- 1 / theta
  lnorm <- log_lik_norm(x = x, mean = mean, sd = sd)
  lstudent <- (lgamma((df + 1) / 2) - lgamma(df / 2) - 0.5 * log(pi * df) - log(sd)) -
    ((df + 1) / 2 * log(1 + (1 / df) * ((x - mean) / sd)^2))
  if (length(theta) == 1) {
    theta <- rep(theta, length(lnorm))
  }
  use_norm <- (!is.na(theta) & theta == 0) | (!is.na(sd) & sd == 0)
  lstudent[use_norm] <- lnorm[use_norm]
  lstudent
}
