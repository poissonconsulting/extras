#' Beta-Binomial Random Samples
#'
#' This parameterization of the beta-binomial distribution uses an expected probability parameter, `prob`, and a dispersion parameter, `theta`. The parameters of the underlying beta mixture are `alpha = (2 * prob) / theta` and `beta = (2 * (1 - prob)) / theta`. This parameterization of `theta` is unconventional, but has useful properties when modelling. When `theta = 0`, the beta-binomial reverts to the binomial distribution. When `theta = 1` and `prob = 0.5`, the parameters of the beta distribution become `alpha = 1` and `beta = 1`, which correspond to a uniform distribution for the beta-binomial probability parameter.
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

# Cumulative distribution function for underdispersed poisson distribution
pupois <- function(q, lambda, theta) {
  sapply(q, \(x) {sum(exp(log_lik_upois(0:x, lambda, theta)))})
  # mapply(
  #   \(q, lambda, theta) {
  #     sum(exp(log_lik_upois(0:q, lambda, theta)))
  #     },
  #   q = q,
  #   lambda = lambda,
  #   theta = theta
  # )
}

#' Underdispersed Poisson Random Samples
#'
#' @inheritParams params
#' @return A numeric vector of the random samples.
#' @family ran_dist
#' @export
#'
#' @examples
#' ran_upois(n = 10, lambda = 1, theta = 1)
ran_upois <- function(n = 1, lambda = 1, theta = 0) {
  chk_whole_number(n)
  chk_gte(n)
  chk_gte(lambda)
  chk_gte(theta)
  # chk_compatible_lengths(rep(1, n), lambda, theta)
  ### Figure out how to get this to work for a lambda/theta vector

  use_pois <- all(theta == 0 & !is.na(theta))
  if (use_pois) {
    stats::rpois(n, lambda = lambda)
  }

  fun <- function(n, lambda, theta) {
    u = stats::runif(n)
    cmf <- pupois(0:max((ceiling(lambda) * 3), 50), lambda = lambda, theta = theta)
    ix <- min(which(abs(cmf - 1) < 1e-8))
    # ix <- min(which(cmf == 1))
    first = min(which(cmf > 0))
    cmf = unique(c(0, cmf[first:ix]))
    cmfTbl = table(cut(u, breaks = cmf, include.lowest = TRUE))
    X = rep(1:length(cmfTbl), as.numeric(cmfTbl)) - 1 + ifelse(first > 1, first, 0)
    X <- as.integer(X)
    samp <- sample(1:n, size = n)
    X[samp]
  }

  lambda_vec <- length(lambda) > 1L & length(unique(lambda)) != 1
  theta_vec <- length(theta) > 1L & length(unique(theta)) != 1
  if (lambda_vec | theta_vec) {
    n_rep <- rep(1, n)
    mapply(
      fun,
      n = n_rep,
      lambda = lambda,
      theta = theta
    )
  } else {
    fun(n, lambda[1], theta[1])
  }
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
  as.integer(stats::rnbinom(n = n, mu = lambda, size = 1/theta))
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
#' ran_student(10, theta = 1/2)
ran_student <- function(n = 1, mean = 0, sd = 1, theta = 0) {
  chk_whole_number(n)
  if (length(mean) > n) {mean = mean[1:n]}
  if (length(sd) > n) {sd = sd[1:n]}
  df <- 1 / theta
  x <- stats::rt(n, df)
  r <- x * sd + mean
  r
}
