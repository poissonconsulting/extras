#' Region of Practical Equivalence
#'
#' Calculates the proportion of the posterior (or credible interval if
#' `level < 1`) that falls within the region of practical equivalence (ROPE)
#' to a `threshold`.
#'
#' @param x A numeric vector of MCMC samples.
#' @param threshold A number specifying the center of the ROPE.
#' @param interval A numeric vector of length 2 to be added to `threshold` to
#' calculate the ROPE.
#' Generally, `threshold[1] == - threshold[2]` and `threshold[1] < 0` should
#' both be true.
#' @param level A number > 0 and <= 1 specifying the probability coverage of
#' the interval to use.
#' The default of 1 uses the full posterior.
#' @param ... Currently unused.
#' @param type A string indicating which type of CI to return.
#' Currently allows Highest Density Intervals (`"HDI"`; default) and
#' Equal-Tailed Intervals (`"ETI"`).
#' @param na_rm A flag indicating whether to remove missing values.
#' @return A number indicating the proportion of the posterior (or credible
#' interval) within the region of practical equivalence.
#' @details
#' The ROPE is calculated as `threshold + interval`.
#' Note that the default is not appropriate for all models, since the `interval`
#' is sensitive to unit conversions.
#'
#' @export
#' @seealso [extras::xtr_ci]
#' @examples
#' xtr_rope(c(-Inf, -1, -0.1, 0.1, 1, Inf))
#' xtr_rope(rnorm(1e4))
xtr_rope <- function(x, threshold = 0, interval = c(-0.1, 0.1), ..., level = 1,
                     type = "HDI", na_rm = FALSE) {
  chk_numeric(x)
  chk_number(threshold)
  chk_numeric(interval)
  chk_length(interval, 2)
  chk_not_any_na(interval)
  chk_sorted(interval)
  chk_unused(...)
  chk_number(level)
  chk_range(level)
  chk_gt(level, 0)
  chk_flag(na_rm)

  if (anyNA(x)) {
    if (na_rm) {
      x <- x[!is.na(x)]
    } else {
      return(NA_real_)
    }
  }

  if (length(x) < 2) {
    return(NA_real_)
  }

  if (level < 1) {
    ci <- xtr_ci(x = x, level = level, ..., type = type, na_rm = na_rm)
    x <- x[x >= ci$lower[1] & x <= ci$upper[1]]
  }
  mean(x >= threshold + interval[1] & x <= threshold + interval[2])
}
