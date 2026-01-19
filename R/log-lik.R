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
#' @param memoize Whether or not to memoize the function.
#'
#' @return An numeric vector of the corresponding log-likelihoods.
#' @family log_lik_dist
#' @export
#'
#' @examples
#' log_lik_beta_binom(c(0, 1, 2), 3, 0.5, 0)
log_lik_beta_binom <- function(x, size = 1, prob = 0.5, theta = 0, memoize = FALSE) {
  alpha <- prob * 2 * (1 / theta)
  beta <- (1 - prob) * 2 * (1 / theta)

  # Memoise use case:
  # Posterior_predictive_check calls this function repeatedly with
  # x and size unchanged; memoize it to reduce repeated calls
  # when length(x) is large enough to outweigh the overhead required by memoize.
  # For length(x) < 800, memoize is slower
  # See detail here https://github.com/poissonconsulting/extras/issues/63
  if (memoize && length(x) >= 800) {
    lgamma_size_x <- lgamma_size_x(size, x)
  } else {
    lgamma_size_x <- lgamma(size + 1) - lgamma(x + 1) - lgamma(size - x + 1)
  }
  lbeta_binom <- lgamma_size_x +
    lgamma(x + alpha) + lgamma(size - x + beta) - lgamma(size + alpha + beta) +
    lgamma(alpha + beta) - lgamma(alpha) - lgamma(beta)

  args_na <- is.na(x + size + prob + theta)
  length_args_na <- length(args_na)
  if (length_args_na == 1) {
    if (args_na) {
      return(NA_real_)
    }
    if (prob == 0) {
      if (x == 0) {
        lbeta_binom <- 0
      } else {
        lbeta_binom <- -Inf
      }
    } else if (prob == 1) {
      if (x == size) {
        lbeta_binom <- 0
      } else {
        lbeta_binom <- -Inf
      }
    }
    if (x > size) {
      lbeta_binom <- -Inf
    }
    if (theta == 0) {
      lbeta_binom <- log_lik_binom(x = x, size = size, prob = prob)
    } else if (theta < 0) {
      lbeta_binom <- NaN
    }
    lbeta_binom
  } else if (length_args_na == 0) {
    numeric(0)
  } else {
    args_not_na <- !args_na
    lbeta_binom[args_not_na & ((x == 0 & prob == 0) | (x == size & prob == 1))] <- 0
    lbeta_binom[args_not_na & x != 0 & prob == 0] <- -Inf
    lbeta_binom[args_not_na & x != size & prob == 1] <- -Inf
    lbeta_binom[args_not_na & x > size] <- -Inf
    theta_not_na <- !is.na(theta)
    lbeta_binom[theta_not_na & theta < 0] <- NaN
    use_binom <- theta_not_na & theta == 0
    if (any(use_binom)) {
      lbinom <- log_lik_binom(x, size = size, prob = prob)
      lbeta_binom[use_binom] <- lbinom[use_binom]
    }
    lbeta_binom
  }
}

# Function to memoize (called repeatedly for non-changing values of size and x)
lgamma_size_x <- function(size, x) {
  lgamma(size + 1) - lgamma(x + 1) - lgamma(size - x + 1)
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

#' Beta Log-Likelihood
#'
#' @inheritParams params
#' @param x A vector of values between 0s and 1s.
#'
#' @return An numeric vector of the corresponding log-likelihoods.
#' @family log_lik_dist
#' @export
#'
#' @examples
#' log_lik_beta(c(0, 0.5, 0.7, 1), 0.7)
log_lik_beta <- function(x, beta = 1, alpha = 1) {
  stats::dbeta(x, shape1 = alpha , shape2 = beta, log = TRUE)
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
#' @examplesIf rlang::is_installed("sn")
#' log_lik_skewnorm(c(-2:2))
#' log_lik_skewnorm(c(-2:2), shape = -2)
#' log_lik_skewnorm(c(-2:2), shape = 2)
log_lik_skewnorm <- function(x, mean = 0, sd = 1, shape = 0) {
  rlang::check_installed("sn")
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
