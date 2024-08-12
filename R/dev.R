# Define a custom optimize function in R
optimize_R <-
  custom_optimize <- function(f, interval, tol = .Machine$double.eps^0.25, maximum = FALSE) {
    lower <- interval[1]
    upper <- interval[2]

    # Golden ratio
    gr <- (sqrt(5) - 1) / 2

    # Initial points
    x1 <- lower + (1 - gr) * (upper - lower)
    x2 <- lower + gr * (upper - lower)
    f1 <- f(x1)
    f2 <- f(x2)

    # Iteratively narrow the search interval
    while (abs(upper - lower) > tol * (abs(x1) + abs(x2))) {
      if ((f1 < f2) != maximum) {
        upper <- x2
        x2 <- x1
        f2 <- f1
        x1 <- lower + (1 - gr) * (upper - lower)
        f1 <- f(x1)
      } else {
        lower <- x1
        x1 <- x2
        f1 <- f2
        x2 <- lower + gr * (upper - lower)
        f2 <- f(x2)
      }
    }

    # Return the minimum (or maximum) point and the function value at that point
    if (f1 < f2) {
      return(list(minimum = x1, objective = f1))
    } else {
      return(list(minimum = x2, objective = f2))
    }
  }

# Function to optimize a vector of parameter values at the same time
# Modified from `optimize_R()` above.
parallel_optimize <- function(f, interval, N, tol = .Machine$double.eps^0.5) {
  if (N == 0) {
    return(numeric(0))
  }

  lower <- rep(interval[1], N)
  upper <- rep(interval[2], N)
  threshold <- (upper - lower) * tol

  # Golden ratio
  gr <- (sqrt(5) - 1) / 2

  # Initial points
  x1 <- lower + (1 - gr) * (upper - lower)
  x2 <- lower + gr * (upper - lower)
  f1 <- f(x1)
  f2 <- f(x2)
  stopifnot(!anyNA(f1), !anyNA(f2))

  # Iteratively narrow the search interval
  while (all((abs(upper - lower) > threshold) | (abs(f2 - f1) > pmax(abs(f1) * tol, 1e-150)))) {
    pos_smaller <- f1 < f2
    idx_smaller <- which(pos_smaller)
    idx_larger <- which(!pos_smaller)

    upper[idx_smaller] <- x2[idx_smaller]
    x2[idx_smaller] <- x1[idx_smaller]
    f2[idx_smaller] <- f1[idx_smaller]
    x1[idx_smaller] <- lower[idx_smaller] + (1 - gr) * (upper[idx_smaller] - lower[idx_smaller])

    lower[idx_larger] <- x1[idx_larger]
    x1[idx_larger] <- x2[idx_larger]
    f1[idx_larger] <- f2[idx_larger]
    x2[idx_larger] <- lower[idx_larger] + gr * (upper[idx_larger] - lower[idx_larger])

    x_new <- ifelse(pos_smaller, x1, x2)
    f_new <- f(x_new)
    f1[idx_smaller] <- f_new[idx_smaller]
    f2[idx_larger] <- f_new[idx_larger]
  }

  x_inside <- x1
  x_inside[idx_larger] <- x2[idx_larger]
  f_inside <- f1
  f_inside[idx_larger] <- f2[idx_larger]

  x_inside <- ifelse(pos_smaller, x1, x2)
  f_inside <- ifelse(pos_smaller, f1, f2)

  x_outside <- ifelse(pos_smaller, lower, upper)
  f_outside <- f(x_outside)

  return(ifelse(f_inside < f_outside, x_inside, x_outside))
}

#' Beta-Binomial Deviances
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
#' @return An numeric vector of the corresponding deviances or deviance
#'   residuals.
#' @family dev_dist
#' @export
#'
#' @examples
#' dev_beta_binom(c(0, 1, 2), 10, 0.5, 0.1)
dev_beta_binom <- function(x, size = 1, prob = 0.5, theta = 0, res = FALSE) {
  force(x)
  args_not_na <- !is.na(x + size + theta)
  if (length(size) == 1) {
    size_vec <- size
    size_rep <- rep(size, length(x))
  } else {
    size_vec <- size[args_not_na]
    size_rep <- size
  }
  if (length(theta) == 1) {
    theta_vec <- theta
    theta_rep <- rep(theta, length(x))
  } else {
    theta_vec <- theta[args_not_na]
    theta_rep <- theta
  }
  opt_p <- rep(NA, length(args_not_na))
  opt_p[args_not_na] <- parallel_optimize(
    f = make_opt_beta_binom(x[args_not_na], size_vec, theta_vec),
    interval = c(0, 1),
    N = sum(args_not_na)
  )
  dev1 <- log_lik_beta_binom(x = x, size = size, prob = opt_p, theta = theta)
  dev2 <- log_lik_beta_binom(x = x, size = size, prob = prob, theta = theta)
  dev <- dev1 - dev2
  dev[dev < 0 & dev > -1e-7] <- 0
  dev <- dev * 2
  use_binom <- (!is.na(theta_rep) & theta_rep == 0) |
    (!is.na(x) & !is.na(size_rep) & x == 0 & size_rep == 0)
  dev_binom <- dev_binom(x = x, size = size, prob = prob, res = FALSE)
  dev[use_binom] <- dev_binom[use_binom]
  if (vld_false(res)) {
    return(dev)
  }
  dev_res(x, size_rep * prob, dev)
}

# Objective function for optimization.
make_opt_beta_binom <- function(x, size, theta) {
  force(x)
  force(size)
  force(theta)
  function(prob) {
    out <- -log_lik_beta_binom(
      x = x,
      prob = prob,
      size = size,
      theta = theta,
      memoize = TRUE
    )
    out[is.nan(out)] <- Inf
    out
  }
}

#' Bernoulli Deviances
#'
#' @inheritParams params
#' @param x A vector of 0s and 1s.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family dev_dist
#' @export
#'
#' @examples
#' dev_bern(c(TRUE, FALSE), 0.7)
dev_bern <- function(x, prob = 0.5, res = FALSE) {
  dev <- ifelse(x == 1, -log(prob), -log(1 - prob))
  dev <- dev * 2
  if (vld_false(res)) {
    return(dev)
  }
  dev_res(x, prob, dev)
}

#' Binomial Deviances
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family dev_dist
#' @export
#'
#' @examples
#' dev_binom(c(0, 1, 2), 2, 0.3)
dev_binom <- function(x, size = 1, prob = 0.5, res = FALSE) {
  mu <- size * prob
  dev1 <- x * log(x / mu)
  dev2 <- (size - x) * log((size - x) / (size - mu))
  dev1[!is.na(x) & x == 0] <- 0
  dev2[!is.na(x) & x == size] <- 0
  dev <- dev1 + dev2
  dev <- dev * 2
  if (vld_false(res)) {
    return(dev)
  }
  dev_res(x, mu, dev)
}

#' Gamma Deviances
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family dev_dist
#' @export
#'
#' @examples
#' dev_gamma(c(0, 1, 2), 1, 2)
dev_gamma <- function(x, shape = 1, rate = 1, res = FALSE) {
  mu <- shape / rate
  dev <- -log(x / mu) + ((x - mu) / mu)
  dev <- 2 * dev
  if (vld_false(res)) {
    return(dev)
  }
  dev_res(x, mu, dev)
}

#' Gamma-Poisson Deviances
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family dev_dist
#' @export
#'
#' @examples
#' dev_gamma_pois(c(1, 3, 4), 3, 2)
dev_gamma_pois <- function(x, lambda = 1, theta = 0, res = FALSE) {
  dev1 <- 1 / theta * log((1 + lambda * theta) / (1 + x * theta))
  dev2 <- x * log((lambda + x * lambda * theta) / (x + x * lambda * theta))
  dev2[!is.na(x) & x == 0] <- 0
  dev <- dev1 - dev2
  dev <- dev * 2
  theta0 <- !is.na(theta) & theta == 0
  dev[theta0] <- dev_pois(x[theta0], lambda[theta0], res = FALSE)
  if (vld_false(res)) {
    return(dev)
  }
  dev_res(x, lambda, dev)
}

#' Zero-Inflated Gamma-Poisson Deviances
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family dev_dist # make live when complete
#' @export
#'
#' @examples
#' dev_gamma_pois_zi(c(1, 3, 4), 3, 2)
dev_gamma_pois_zi <- function(x, lambda = 1, theta = 0, prob = 0, res = FALSE) {
  dev <- log_lik_gamma_pois_zi(x, lambda = x, theta = theta, prob = 0) -
    log_lik_gamma_pois_zi(x, lambda = lambda, theta = theta, prob = prob)
  dev <- dev * 2
  if (vld_false(res)) {
    return(dev)
  }
  dev_res(x, lambda * (1 - prob), dev)
}

#' Log-Normal Deviances
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family dev_dist
#' @export
#'
#' @examples
#' dev_lnorm(exp(-2:2))
dev_lnorm <- function(x, meanlog = 0, sdlog = 1, res = FALSE) {
  x <- pmax(x, 0)
  dev_norm(log(x), mean = meanlog, sd = sdlog, res = res)
}

#' Negative Binomial Deviances
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family dev_dist
#' @export
#'
#' @examples
#' dev_neg_binom(c(1, 2, 5), 2, 3)
dev_neg_binom <- function(x, lambda = 1, theta = 0, res = FALSE) {
  dev_gamma_pois(x, lambda = lambda, theta = theta, res = res)
}

#' Normal Deviances
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family dev_dist
#' @export
#'
#' @examples
#' dev_norm(c(-2:2))
dev_norm <- function(x, mean = 0, sd = 1, res = FALSE) {
  dev <- ((x - mean) / sd)^2
  if (vld_false(res)) {
    return(dev)
  }
  dev_res(x, mean, dev)
}

#' Poisson Deviances
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family dev_dist
#' @export
#'
#' @examples
#' dev_pois(c(1, 3, 4), 3)
dev_pois <- function(x, lambda, res = FALSE) {
  dev <- x * log(x / lambda) - (x - lambda)
  zero <- !is.na(x) & x == 0
  if (any(zero)) {
    if (length(lambda) == 1) {
      lambda <- rep(lambda, length(x))
    }
    dev[zero] <- lambda[zero]
  }
  dev <- dev * 2
  if (vld_false(res)) {
    return(dev)
  }
  dev_res(x, lambda, dev)
}

#' Zero-Inflated Poisson Deviances
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family dev_dist
#' @export
#'
#' @examples
#' dev_pois_zi(c(1, 3, 4), 3)
dev_pois_zi <- function(x, lambda, prob = 0, res = FALSE) {
  dev1 <- -x + x * log(x) - log(factorial(x))
  dev1[x == 0] <- 0
  dev0 <- log(prob + (1 - prob) * exp(-lambda))
  devg0 <- log(1 - prob) - lambda + x * log(lambda) - log(factorial(x))
  dev <- dev1 - dev0
  devg0 <- dev1 - devg0
  g0 <- !is.na(x) & x > 0
  dev[g0] <- devg0[g0]
  dev <- dev * 2
  if (vld_false(res)) {
    return(dev)
  }
  dev_res(x, lambda * (1 - prob), dev)
}


#' Skew Normal Deviances
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#' @param shape A numeric vector of shape.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family dev_dist
#' @export
#'
#' @examples
#' dev_skewnorm(c(-2:2))
#' dev_skewnorm(-2:2, 0, 1, 5)
#' dev_skewnorm(-2:2, 0, 1, 5, res = TRUE)
dev_skewnorm <- function(x, mean = 0, sd = 1, shape = 0, res = FALSE) {
  delta <- shape / sqrt(1 + shape^2)
  mu_z <- sqrt(2 / pi) * delta
  sig_z <- sqrt(1 - mu_z^2)
  gam_1 <- ((4 - pi) / 2) *
    ((delta * sqrt(2 / pi))^3 / (1 - (2 * delta^2) / pi)^(3 / 2))
  m_o <- mu_z - (gam_1 * sig_z / 2) - (sign(shape) / 2) * exp(-2 * pi / abs(shape))
  mode_sat <- mean + sd * m_o
  dev <- log_lik_skewnorm(mode_sat, mean = mean, sd = sd, shape = shape) -
    log_lik_skewnorm(x, mean = mean, sd = sd, shape = shape)
  neg <- dev < 0
  dev[neg] <- 0
  dev <- dev * 2
  if (vld_false(res)) {
    return(dev)
  }
  dev_res(x, mean + sd * (shape / sqrt(1 + shape^2)) * sqrt(2 / pi), dev)
}

#' Student's t Deviances
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family dev_dist
#' @export
#'
#' @examples
#' dev_student(c(1, 3.5, 4), 3)
dev_student <- function(x, mean = 0, sd = 1, theta = 0, res = FALSE) {
  dev <- log_lik_student(x, mean = x, sd = sd, theta = theta) -
    log_lik_student(x, mean = mean, sd = sd, theta = theta)
  dev <- dev * 2
  if (vld_false(res)) {
    return(dev)
  }
  dev_res(x, mean, dev)
}
