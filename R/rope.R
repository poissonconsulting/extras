#' Region of Practical Equivalence
#'
#' Calculates the proportion of the posterior (or credible interval if
#' `level < 1`) that falls within the region of practical equivalence (ROPE)
#' to a `threshold`.
#' The ROPE is calculated as `threshold + interval`.
#' Note that the default is not appropriate for all models, since the `interval`
#' is sensitive to unit conversions.
#'
#' @param x A numeric vector of MCMC samples.
#' @param level A number > 0 and <= 1 specifying the probability coverage of
#' the HDI to use.
#' The default of 1 uses the full posterior.
#' @param ... Currently unused.
#' @param na_rm A flag indicating whether to remove missing values.
#' @return A number indicating the estimated proportion of the posterior (or
#' credible interval) within the region of practical equivalence.
#' @export
#' @seealso [extras::xtr_ci]
#' @examples
#' rope(c(-Inf, -1, -0.1, 0.1, 1, Inf))
#' rope(rnorm(1e4))
#' @name rope
NULL

rope <- function(x, threshold = 0, interval = c(-0.1, 0.1), ..., level = 1,
                 type = "HDI", na_rm = FALSE) {
  chk_numeric(x)
  chk_number(threshold)
  chk_numeric(interval)
  chk_length(interval, 2)
  chk_unused(...)
  chk_number(level)
  chk_range(level)
  chk_flag(na_rm)

  if (anyNA(x)) {
    if (vld_true(na_rm)) {
      x <- x[!is.na(x)]
    } else {
      NA_real_
    }
  }

  if(level < 1) {
    ci <- xtr_ci(x = x, level = level, ..., type = type, na_rm = na_rm)
    x <- x[x >= ci[1] & x <= ci[2]]
  }
  mean(x >= threshold + interval[1] & x <= threshold + interval[2])
}
