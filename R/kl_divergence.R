#' Kullback–Leibler divergence
#'
#' The Kullback–Leibler (KL) divergence (or relative entropy or I-divergence)
#' measures how different a probability distribution \eqn{P(x)} is from a
#' reference one \eqn{Q(x)} as \deqn{{D_{\text{KL}}(P\parallel Q) = \sum_{x\in {\mathcal {X}}} P(x)\,\log {\frac {P(x)}{Q(x)}}{\text{.}}}} Typically, \eqn{P(x)} is assumed to be a
#' correct distribution, but this function uses a given observed distribution
#' and contrasts it to a potential approximation of such distribution
#' (\eqn{Q(x)}).
#'
#' @param x A numeric vector of MCMC values.
#' @param distribution Density function for the reference distribution.
#' Common examples include [`dnorm()`], [`dt()`], `[dexp()]`, and [`dgamma()`].
#' Continuous distributions are discretized into histograms using
#' `log2(length(x)) + 1` bins.
#' @param ... Used to pass (named) parameter arguments to the density function
#' specified by `distribution`.
#' @inheritParams params
#' @param include_zero Logical flag indicating whether to include zero in the bins for the histogram.
#' Defaults to `FALSE`.
#' Setting as `TRUE` sets the minimum to zero for strictly positive values of
#' `x` and sets the maximum to zero for strictly negative values of `x`.
#' This argument is ignored if zero is inside the interval given by `range(x)`.
#' @return A numeric vector of length one
#' @family summary
#' @export
#' @examples
#' kl_divergence(1:10)
#' kl_divergence(rnorm(100))
#' kl_divergence(rnorm(100, 3, 4))
#' kl_divergence(rnorm(100, 3, 4), ref_pars = c(3, 4))
#' kl_divergence(rt(1000, df = 2))
#' kl_divergence(rt(1000, df = 2), dt, ref_pars = 2)
#' kl_divergence(rt(1000, df = 10) - 5, function(.x, .df) dt(.x + 5, df = .df),
#'               ref_pars = c(10))

kl_divergence <- function(x = x, distribution = dnorm, ref_pars = c(0, 1),
                          range = c(-Inf, Inf), na_rm = FALSE, plots = TRUE) {
  # TODO: remove `plots` arguement
  # TODO: allow comparing two samples nonparametrically
  # TODO: make `kl_normal`, `kl_lnorm`, `kl_exp`, ..., for all families
  # TODO: `kl_samples` for two nonparametric samples
  chk_numeric(x)
  chk_function(distribution)
  chk_numeric(ref_pars)
  chk_true(length(ref_pars) <= 2)
  chk_numeric(range)
  chk_length(range, 2)
  chk_flag(na_rm)

  if (na_rm) {
    x <- x[!is.na(x)]
  }
  if (any(is.na(x))) {
    return(NA_real_)
  }
  x <- sort(x)
  x_min <- min(x)
  x_max <- max(x)
  if (is.finite(range[1])) {
    x_min <- range[1]
  }
  if(is.finite(range[2])) {
    x_max <- range[2]
  }
  # the probability function becomes flat if eps is too small or too large
  # the eps seems to be related to the sample size passed to ecdf()
  eps <- 400 / length(x)
  if (diff(range(x)) / eps < 5) {
    return(NA_real_)
  }
  ECDF <- ecdf(x)
  sample_space <- seq(x_min + 2 * eps, x_max - 2 * eps, by = eps / 100)
  left_sample <- sample_space - eps
  right_sample <- sample_space + eps
  left_sample <- pmax(left_sample, x_min)
  right_sample <- pmin(right_sample, x_max)
  left <- ECDF(left_sample)
  right <- ECDF(right_sample)
  p_obs <- (right - left) / (right_sample - left_sample)
  if (length(ref_pars) == 1) {
    p_ref <- distribution(sample_space, ref_pars[1])
  } else {
    p_ref <- distribution(sample_space, ref_pars[1], ref_pars[2])
  }
  if (plots) {
    layout(matrix(c(1:3, 3), ncol = 2, byrow = TRUE))
    plot(sample_space, p_obs, type = "l"); plot(sample_space, p_ref,type= "l")
    plot(p_ref, p_obs, type = "l")
    layout(1)
  }
  sum(p_obs * log(p_obs / p_ref))
}

if(FALSE) {
  debug(kl_divergence)
  # TODO: something is wrong with the right side of the density plots
  kl_divergence(qnorm(seq(0.001, 0.995, by = 0.001)))
  kl_divergence(qnorm(seq(0.001, 0.995, length.out = 400)))
  kl_divergence(qnorm(seq(0.001, 0.995, length.out = 350)))
  kl_divergence(qexp(seq(0.001, 0.995, by = 0.0001)), range = c(0, Inf))
  kl_divergence(qnorm(seq(0.001, 0.995, by = 0.0001), mean = 3), range = c(0, Inf))
  undebug(kl_divergence)
}
