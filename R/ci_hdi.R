#' Highest Density Interval
#'
#' Calculates Bayesian credible intervals using the highest density
#' interval (HDI), i.e., the narrowest CI with the specified minimum coverage.
#'
#' @param x A numeric vector of MCMC samples.
#' @param level A number > 0 and <= 1 specifying the probability coverage of the
#'  HDI.
#' @param ... Currently unused.
#' @param na_rm A flag indicating whether to remove missing values.
#' @return A [data.frame] of the `lower` and `upper` limits for the credible
#' interval.
#' Note that the interval is not guaranteed to be one-sided or two-sided.
#' Returns integer limits if the input data are integers and double otherwise.
#' @export
#' @seealso [extras::xtr_ci()] and [extras::xtr_ci_eti()]
#' @examples
#' xtr_ci_hdi(1:10, level = 0.1) # only 10% of values inside
#' xtr_ci_hdi(1:10, level = 0.2) # only 20% of values inside
#' xtr_ci_hdi(1:10, level = 0.2 + 0.01) # at least 20.1% of values inside
#' xtr_ci_hdi(1:100) # inclusive interval [3, 98] with 5% of values outside
xtr_ci_hdi <- function(x, level = 0.95, ..., na_rm = FALSE) {
  chk_numeric(x)
  chk_number(level)
  chk_range(level, inclusive = TRUE)
  chk_gt(level)
  chk_unused(...)
  chk_flag(na_rm)

  if(is.integer(x)) {
    na <- NA_integer_
  } else {
    na <- NA_real_
  }

  if (anyNA(x)) {
    if (vld_true(na_rm)) {
      x <- x[!is.na(x)]
    } else {
      return(data.frame(lower = na, upper = na))
    }
  }

  x <- sort(x)
  n <- length(x)

  if (n <= 1) {
    return(data.frame(lower = na, upper = na))
  }

  if (level == 1) {
    return(data.frame(lower = x[1], upper = x[n]))
  }

  n_in <- ceiling(n * level)
  n_out <- n - n_in
  n_inf <- sum(is.infinite(x))
  widths <- sapply(1:(n_out + 1), function(.i) {
    x[.i + n_in - 1] - x[.i]
  })
  narrowest_is <- which(widths == min(widths))

  if (length(narrowest_is) == 1) {
    narrowest_i <- narrowest_is
  } else {
    if (n_inf <= n_out) {
      actual_n_ins <- sapply(narrowest_is, function(.i) {
        .l <- x[.i + n_in - 1]
        .u <- x[.i]
        sum(x >= .l & x <= .u)
      })
      narrowest_is <- narrowest_is[actual_n_ins == max(actual_n_ins)]
      midpoints <- narrowest_is + (n_in - 1) / 2
      narrowest_i <- narrowest_is[which.min(abs(midpoints - (1 + n) / 2))]
    } else if(n_inf < n_in) {
      if (is.infinite(x[1])) {
        narrowest_i <- min(narrowest_is)
      } else {
        narrowest_i <- max(narrowest_is)
      }
    } else {
      return(data.frame(lower = x[1], upper = x[n]))
    }
  }
  l <- x[narrowest_i]
  u <- x[n_in + narrowest_i - 1]

  data.frame(lower = l, upper = u)
}
