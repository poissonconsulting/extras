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
                          na_rm = FALSE, include_zero = FALSE) {
  chk_numeric(x)
  chk_function(distribution)
  chk_numeric(ref_pars)
  chk_true(length(ref_pars) <= 2)
  chk_logical(na_rm)
  chk_logical(include_zero)

  if (na_rm) {
    x <- x[!is.na(x)]
  }
  if (any(is.na(x))) {
    return(NA_real_)
  }
  x_min <- min(x)
  x_max <- max(x)
  if (include_zero) {
    if(x_min > 0) {
      x_min <- 0
    }
    if(x_max < 0) {
      x_max <- 0
    }
  }
  n_bins <- floor(log2(length(x)) + 1)
  bins <- seq(min(x), max(x), length.out = n_bins)
  p_obs <- sapply(1:(n_bins - 1), function(.i) {
    # include max(x) in the last bin
    if (.i < n_bins - 1) {
      mean(x >= bins[.i] & x < bins[.i])
    } else {
      mean(x >= bins[.i] & x <= bins[.i])
    }
  })
  if (length(ref_pars) == 1) {
    p_ref <- distribution((bins[1:(n_bins - 1)] + bins[2:n_bins]) / 2,
                          ref_pars[1])
  } else {
    p_ref <- distribution((bins[1:(n_bins - 1)] + bins[2:n_bins]) / 2,
                          ref_pars[1], ref_pars[2])
  }
  weighted.mean(p_obs * log(p_obs / p_ref), w = p_obs * n_bins)
}
