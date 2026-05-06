#' Highest Density Interval
#'
#' Calculates Bayesian credible intervals using the highest density
#' interval (HDI), i.e., the narrowest CI with the specified minimum coverage.
#'
#' @param x A numeric vector of MCMC samples.
#' @param level A number between 0 and 1 (exclusive) specifying the probability
#' coverage of the HDI.
#' @param ... Currently unused.
#' @param na_rm A flag indicating whether to remove missing values.
#' @return A [tibble::tibble] of the `lower` and `upper` limits for the credible interval.
#' Note that the interval is not guaranteed to be one-sided or two-sided.
#' @export
#' @seealso [extras::xtr_ci()] and [extras::xtr_ci_eti()]
#' @examples
#' xtr_ci_hdi(rnorm(1e4))
#' @name xtr_ci_hdi
NULL

xtr_ci_hdi <- function(x, level = 0.95, ..., na_rm = FALSE) {
  chk_numeric(x)
  chk_number(level)
  chk_range(level)
  chk_flag(na_rm)
  chk_unused(...)

  if (anyNA(x)) {
    if (vld_true(na_rm)) {
      x <- x[!is.na(x)]
    } else {
      return(tibble::tibble(lower = NA_real_, upper = NA_real_))
    }
  }
  x <- sort(x)
  n <- length(x)

  if (n < 1 / (1 - level)) {
    return(tibble::tibble(lower = NA_real_, upper = NA_real_))
  }

  n_in <- ceiling(n * level)
  n_out <- n - n_in

  if (sum(is.infinite(x)) >= n_in) {
    return(tibble::tibble(lower = Inf * sign(min(x[is.infinite(x)])),
                      upper = Inf * sign(max(x[is.infinite(x)]))))
  }

  widths <- sapply(1:n_out, function(.i) x[.i + n_in] - x[.i])
  narrowest_is <- which(widths == min(widths)) # which.min() returns first min

  if (length(narrowest_is) == 1) {
    narrowest_i <- narrowest_is
  } else {
    # not testing for non-consecutive intervals: seems inconsequential
    narrowest_i <- round(xtr_median(narrowest_is))
  }

  tibble::tibble(
    lower = x[narrowest_i],
    upper = x[narrowest_i + n_in]
  )
}
