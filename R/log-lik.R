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
log_lik_beta_binom <- function(x, size = 1, prob = 0.5, theta = 0, tlower = 0, tupper = Inf, memoize = FALSE) {
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
    log_lik <- lbeta_binom
  } else if (length_args_na == 0) {
    log_lik <- numeric(0)
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
    log_lik <- lbeta_binom
  }
  truncated <- (tlower != 0 | !is.infinite(tupper)) & !is.na(tlower) & !is.na(tupper)
  if (any(truncated & !is.na(truncated))) {
    log_lik_truncated <- log_lik - log(
      prob_beta_binom(tupper, size = size, prob = prob, theta = theta) -
        prob_beta_binom(tlower, size = size, prob = prob, theta = theta) +
        (
          lgamma(size + 1) - lgamma(x + 1) - lgamma(size - x + 1) +
            lgamma(x + alpha) + lgamma(size - x + beta) -
            lgamma(size + alpha + beta) + lgamma(alpha + beta) - lgamma(alpha) -
            lgamma(beta)
        )
    )
    log_lik_truncated[x < tlower | x > tupper] <- -Inf
    log_lik[truncated] <- log_lik_truncated[truncated]
  }
  trunc_na <- is.na(tlower) | is.na(tupper)
  log_lik[trunc_na] <- NA
  log_lik
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
log_lik_binom <- function(x, size = 1, prob = 0.5, tlower = 0, tupper = Inf) {
  log_lik <- dbinom(x, size = size, prob = prob, log = TRUE)
  truncated <- (tlower != 0 | !is.infinite(tupper)) & !is.na(tlower) & !is.na(tupper)
  if (any(truncated & !is.na(truncated))) {
    log_lik_truncated <- log_lik - log(
      prob_binom(tupper, size = size, prob = prob) -
         prob_binom(tlower, size = size, prob = prob) +
        dbinom(x = tlower, size = size, prob = prob)
    )
    log_lik_truncated[x < tlower | x > tupper] <- -Inf
    log_lik[truncated] <- log_lik_truncated[truncated]
  }
  if (any(is.na(tlower), is.na(tupper))) {
    trunc_na <- is.na(tlower) | is.na(tupper)
    log_lik[trunc_na] <- NA
  }
  log_lik
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
log_lik_gamma <- function(x, shape = 1, rate = 1, tlower = 0, tupper = Inf) {
  log_lik <- stats::dgamma(x, shape = shape, rate = rate, log = TRUE)
  truncated <- (tlower != 0 | !is.infinite(tupper)) & !is.na(tlower) & !is.na(tupper)
  if (any(truncated & !is.na(truncated))) {
    log_lik_truncated <- log_lik - log(
      prob_gamma(tupper, shape = shape, rate = rate) -
        prob_gamma(tlower, shape = shape, rate = rate)
    )
    log_lik_truncated[x < tlower | x > tupper] <- -Inf
    log_lik[truncated] <- log_lik_truncated[truncated]
  }
  if (any(is.na(tlower), is.na(tupper))) {
    trunc_na <- is.na(tlower) | is.na(tupper)
    log_lik[trunc_na] <- NA
  }
  log_lik
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
log_lik_gamma_pois <- function(x, lambda = 1, theta = 0, tlower = 0, tupper = Inf) {
  log_lik <- log_lik_neg_binom(x, lambda = lambda, theta = theta)
  truncated <- (tlower != 0 | !is.infinite(tupper)) & !is.na(tlower) & !is.na(tupper)
  if (any(truncated & !is.na(truncated))) {
    log_lik_truncated <- log_lik - log(
      prob_gamma_pois(tupper, lambda = lambda, theta = theta) -
        prob_gamma_pois(tlower, lambda = lambda, theta = theta) +
        dnbinom(x = tlower, mu = lambda, size = 1 / theta)
    )
    log_lik_truncated[x < tlower | x > tupper] <- -Inf
    log_lik[truncated] <- log_lik_truncated[truncated]
  }
  if (any(is.na(tlower), is.na(tupper))) {
    trunc_na <- is.na(tlower) | is.na(tupper)
    log_lik[trunc_na] <- NA
  }
  log_lik
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
log_lik_gamma_pois_zi <- function(x, lambda = 1, theta = 0, prob = 0, tlower = 0, tupper = Inf) {
  if (tlower > 0 & !is.na(tlower)) {
    warning("Specifying a lower truncation point greater than 0 doesn't make sense for a zero-inflated distribution.")
  }
  lpois <- dnbinom(x, mu = lambda, size = 1 / theta)
  lpois <- lpois * (1 - prob)
  zero <- !is.na(x) & x == 0
  if (length(prob) == 1) {
    prob <- rep(prob, length(lpois))
  }
  lpois[zero] <- lpois[zero] + prob[zero]
  log_lik <- log(lpois)
  truncated <- (tlower != 0 | !is.infinite(tupper)) & !is.na(tlower) & !is.na(tupper)
  if (any(truncated & !is.na(truncated))) {
    log_lik_truncated <- log_lik - log(
      prob_gamma_pois_zi(tupper, lambda = lambda, theta = theta, prob = prob) -
        prob_gamma_pois_zi(tlower, lambda = lambda, theta = theta, prob = prob) +
        (
          dnbinom(tlower, mu = lambda, size = 1 / theta) *
            (1 - prob) +
            prob * as.integer(tlower == 0)
        )
    )
    log_lik_truncated[x < tlower | x > tupper] <- -Inf
    log_lik[truncated] <- log_lik_truncated[truncated]
  }
  if (any(is.na(tlower), is.na(tupper))) {
    trunc_na <- is.na(tlower) | is.na(tupper)
    log_lik[trunc_na] <- NA
  }
  log_lik
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
log_lik_lnorm <- function(x, meanlog = 0, sdlog = 1, tlower = 0, tupper = Inf) {
  log_lik <- dlnorm(x, meanlog = meanlog, sdlog = sdlog, log = TRUE)
  truncated <- (tlower != 0 | !is.infinite(tupper)) & !is.na(tlower) & !is.na(tupper)
  if (any(truncated & !is.na(truncated))) {
    log_lik_truncated <- log_lik - log(
      prob_lnorm(tupper, meanlog = meanlog, sdlog = sdlog) -
        prob_lnorm(tlower, meanlog = meanlog, sdlog = sdlog)
    )
    log_lik_truncated[x < tlower | x > tupper] <- -Inf
    log_lik[truncated] <- log_lik_truncated[truncated]
  }
  if (any(is.na(tlower), is.na(tupper))) {
    trunc_na <- is.na(tlower) | is.na(tupper)
    log_lik[trunc_na] <- NA
  }
  log_lik
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
log_lik_neg_binom <- function(x, lambda = 1, theta = 0, tlower = 0, tupper = Inf) {
  log_lik <- dnbinom(x, mu = lambda, size = 1 / theta, log = TRUE)
  truncated <- (tlower != 0 | !is.infinite(tupper)) & !is.na(tlower) & !is.na(tupper)
  if (any(truncated & !is.na(truncated))) {
    log_lik_truncated <- log_lik - log(
      prob_neg_binom(tupper, lambda = lambda, theta = theta) -
        prob_neg_binom(tlower, lambda = lambda, theta = theta) +
        dnbinom(x = tlower, mu = lambda, size = 1 / theta)
    )
    log_lik_truncated[x < tlower | x > tupper] <- -Inf
    log_lik[truncated] <- log_lik_truncated[truncated]
  }
  if (any(is.na(tlower), is.na(tupper))) {
    trunc_na <- is.na(tlower) | is.na(tupper)
    log_lik[trunc_na] <- NA
  }
  log_lik
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
log_lik_norm <- function(x, mean = 0, sd = 1, tlower = -Inf, tupper = Inf) {
  log_lik <- dnorm(x, mean = mean, sd = sd, log = TRUE)
  truncated <- (!is.infinite(tlower) | !is.infinite(tupper)) & !is.na(tlower) & !is.na(tupper)
  if (any(truncated & !is.na(truncated))) {
    log_lik_truncated <- log_lik - log(
      prob_norm(tupper, mean = mean, sd = sd) -
        prob_norm(tlower, mean = mean, sd = sd)
    )
    log_lik_truncated[x < tlower | x > tupper] <- -Inf
    log_lik[truncated] <- log_lik_truncated[truncated]
  }
  if (any(is.na(tlower), is.na(tupper))) {
    trunc_na <- is.na(tlower) | is.na(tupper)
    log_lik[trunc_na] <- NA
  }
  log_lik
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
log_lik_pois <- function(x, lambda = 1, tlower = 0, tupper = Inf) {
  log_lik <- dpois(x, lambda, log = TRUE)
  truncated <- (tlower != 0 | !is.infinite(tupper)) & !is.na(tlower) & !is.na(tupper)
  if (any(truncated & !is.na(truncated))) {
    log_lik_truncated <- log_lik - log(
      prob_pois(tupper, lambda = lambda) -
        prob_pois(tlower, lambda = lambda) +
        dpois(x = tlower, lambda = lambda)
    )
    log_lik_truncated[x < tlower | x > tupper] <- -Inf
    log_lik[truncated] <- log_lik_truncated[truncated]
  }
  if (any(is.na(tlower), is.na(tupper))) {
    trunc_na <- is.na(tlower) | is.na(tupper)
    log_lik[trunc_na] <- NA
  }
  log_lik
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
log_lik_pois_zi <- function(x, lambda = 1, prob = 0, tlower = 0, tupper = Inf) {
  if (any(tlower > 0 & !is.na(tlower))) {
    warning("Specifying a lower truncation point greater than 0 doesn't make sense for a zero-inflated distribution.")
  }
  lpois <- dpois(x, lambda = lambda)
  lpois <- lpois * (1 - prob)
  zero <- x == 0
  lpois[zero] <- lpois[zero] + prob
  log_lik <- log(lpois)
  truncated <- (tlower != 0 | !is.infinite(tupper)) & !is.na(tlower) & !is.na(tupper)
  if (any(truncated & !is.na(truncated))) {
    log_lik_truncated <- log_lik - log(
      prob_pois_zi(tupper, lambda = lambda, prob = prob) -
        prob_pois_zi(tlower, lambda = lambda, prob = prob) +
        (dpois(x = tlower, lambda = lambda) * (1 - prob) * prob * as.integer(tlower == 0))
    )
    log_lik_truncated[x < tlower | x > tupper] <- -Inf
    log_lik[truncated] <- log_lik_truncated[truncated]
  }
  if (any(is.na(tlower), is.na(tupper))) {
    trunc_na <- is.na(tlower) | is.na(tupper)
    log_lik[trunc_na] <- NA
  }
  log_lik
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
log_lik_skewnorm <- function(x, mean = 0, sd = 1, shape = 0, tlower = -Inf, tupper = Inf) {
  rlang::check_installed("sn")
  log_lik <- dskewnorm(x = x, mean = mean, sd = sd, shape = shape, log = TRUE)
  use_norm <- !is.na(shape) & shape == 0
  lnorm <- log_lik_norm(x = x, mean = mean, sd = sd)
  lengths <- as.logical(length(x)) + as.logical(length(mean)) + as.logical(length(sd)) + as.logical(length(shape))
  if (lengths >= 4) log_lik[use_norm] <- lnorm[use_norm]
  truncated <- (!is.infinite(tlower) | !is.infinite(tupper)) & !is.na(tlower) & !is.na(tupper)
  if (any(truncated & !is.na(truncated))) {
    log_lik_truncated <- log_lik - log(
      prob_skewnorm(tupper, mean = mean, sd = sd, shape = shape) -
        prob_skewnorm(tlower, mean = mean, sd = sd, shape = shape)
    )
    log_lik_truncated[x < tlower | x > tupper] <- -Inf
    log_lik[truncated] <- log_lik_truncated[truncated]
  }
  if (any(is.na(tlower), is.na(tupper))) {
    trunc_na <- is.na(tlower) | is.na(tupper)
    log_lik[trunc_na] <- NA
  }
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
log_lik_student <- function(x, mean = 0, sd = 1, theta = 0, tlower = -Inf, tupper = Inf) {
  df <- 1 / theta
  lnorm <- log_lik_norm(x = x, mean = mean, sd = sd)
  log_lik <- (lgamma((df + 1) / 2) - lgamma(df / 2) - 0.5 * log(pi * df) - log(sd)) -
    ((df + 1) / 2 * log(1 + (1 / df) * ((x - mean) / sd)^2))
  if (length(theta) == 1) {
    theta <- rep(theta, length(lnorm))
  }
  use_norm <- (!is.na(theta) & theta == 0) | (!is.na(sd) & sd == 0)
  log_lik[use_norm] <- lnorm[use_norm]
  truncated <- (!is.infinite(tlower) | !is.infinite(tupper)) & !is.na(tlower) & !is.na(tupper)
  if (any(truncated & !is.na(truncated))) {
    log_lik_truncated <- log_lik - log(
      prob_student(tupper, mean = mean, sd = sd, theta = theta) -
        prob_student(tlower, mean = mean, sd = sd, theta = theta)
    )
    log_lik_truncated[x < tlower | x > tupper] <- -Inf
    log_lik[truncated] <- log_lik_truncated[truncated]
  }
  if (any(is.na(tlower), is.na(tupper))) {
    trunc_na <- is.na(tlower) | is.na(tupper)
    log_lik[trunc_na] <- NA
  }
  log_lik
}
