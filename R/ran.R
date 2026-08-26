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
  ran_neg_binom(n = n, lambda = lambda, theta = theta) *
    ran_bern(n, prob = 1 - prob)
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

#' Multinomial Random Samples
#'
#' Models the counts across two or more mutually exclusive categories from a
#' fixed number of trials, in \emph{long} format: one value per category per
#' trial, with `group` identifying which rows belong to the same trial. All
#' rows sharing a `group` must have the same `size`, and their `prob` values
#' must sum to 1.
#'
#' Unlike the other `ran_*()` functions, `ran_multinom()` has no `n`
#' argument: the number of samples is fully determined by `length(prob)`
#' (equivalently `length(group)`), since a trial's categories can't be
#' generated independently of one another.
#'
#' @inheritParams params
#' @param prob A numeric vector of the probability of the category. Must sum
#'   to 1 across the rows sharing the same `group`. `NA` in `size` or `prob`
#'   for any row of a trial makes the sample `NA` for every row of that
#'   trial, since a trial's categories are drawn jointly.
#' @return An integer vector of the random samples, one per row of `prob`.
#' @family ran_dist
#' @references
#' Johnson, N.L., Kotz, S., and Balakrishnan, N. 1997.
#' Discrete Multivariate Distributions. John Wiley and Sons, New York.
#'
#' Gelman, A., Meng, X.-L., and Stern, H. 1996. Posterior predictive
#' assessment of model fitness via realized discrepancies.
#' Statistica Sinica 6(4): 733-807.
#' @export
#'
#' @examples
#' ran_multinom(size = 10, prob = c(0.2, 0.3, 0.5), group = c(1, 1, 1))
ran_multinom <- function(size = 1, prob, group) {
  chk_compatible_lengths(size, prob, group)
  n <- length(prob)
  size <- rep_len(size, n)
  prob <- rep_len(prob, n)
  group <- rep_len(group, n)
  chk_not_any_na(group)
  groups <- multinom_split(group)
  chk_multinom_group(size, prob, group, groups)
  row_na <- multinom_row_na(size, prob, group, groups)
  x <- rep(NA_real_, n)
  for (idx in groups) {
    if (row_na[idx[1]]) {
      next
    }
    x[idx] <- stats::rmultinom(1, size = size[idx[1]], prob = prob[idx])[, 1]
  }
  as.integer(x)
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
  as.integer(stats::rnbinom(n = n, mu = lambda, size = 1 / theta))
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
#' @examplesIf rlang::is_installed("sn")
#' ran_skewnorm(10, shape = -1)
#' ran_skewnorm(10, shape = 0)
#' ran_skewnorm(10, shape = 1)
ran_skewnorm <- function(n = 1, mean = 0, sd = 1, shape = 0) {
  rlang::check_installed("sn")
  chk_whole_number(n)
  chk_gte(n)
  rskewnorm(n = n, mean = mean, sd = sd, shape = shape)
}

#' Skew-Lognormal Random Samples
#'
#' @inheritParams params
#' @param shape A numeric vector of shape.
#' @return A numeric vector of the random samples.
#' @family ran_dist
#' @export
#'
#' @examplesIf rlang::is_installed("sn")
#' ran_skewlnorm(10, shape = -1)
#' ran_skewlnorm(10, shape = 0)
#' ran_skewlnorm(10, shape = 1)
ran_skewlnorm <- function(n = 1, meanlog = 0, sdlog = 1, shape = 0) {
  rlang::check_installed("sn")
  chk_whole_number(n)
  chk_gte(n)
  rskewlnorm(n = n, meanlog = meanlog, sdlog = sdlog, shape = shape)
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
ran_student <- function(n = 1, mean = 0, sd = 1, theta = 0) {
  chk_whole_number(n)
  if (length(mean) > n) {
    mean <- mean[1:n]
  }
  if (length(sd) > n) {
    sd <- sd[1:n]
  }
  df <- 1 / theta
  x <- stats::rt(n, df)
  r <- x * sd + mean
  r
}
