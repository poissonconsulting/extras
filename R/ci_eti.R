#' Equal-Tailed Interval
#'
#' Calculates Bayesian credible intervals using the equal-tailed interval (ETI),
#' i.e., the CI such that the left and right tails outside the CI have the same
#' coverage.
#'
#' @inheritParams params
#' @param x A numeric vector of MCMC samples.
#' @param ... Currently unused.
#' @param na_rm A flag indicating whether to remove missing values.
#' @return A [data.frame] of the `lower` and `upper` limits for the credible
#' interval.
#'
#' @details
#' The interval is guaranteed to be two-sided, unlike `[xtr_ci_hdi()]`.
#' Does not return integer outputs even if the input data are integers,
#' unlike [`xtr_ci_hdi()`].
#' The interval limits are always real (double) numeric values.
#'
#' @export
#' @seealso [extras::xtr_ci()] and [extras::xtr_ci_hdi()]
#' @examples
#' xtr_ci_eti(rnorm(1e4))
xtr_ci_eti <- function(x, level = 0.95, ..., na_rm = FALSE) {
  chk_numeric(x)
  chk_number(level)
  chk_range(level, inclusive = TRUE)
  chk_gt(level)
  chk_unused(...)
  chk_flag(na_rm)

na <-  if (is.integer(x)) NA_integer_ else NA_real_

  if (length(x) <= 1) {
    return(data.frame(lower = na, upper = na))
  }

  if (anyNA(x)) {
    if (na_rm) {
      x <- x[!is.na(x)]
    } else {
      return(data.frame(lower = na, upper = na))
    }
  }

  data.frame(
    lower = unname(stats::quantile(x, (1 - level) / 2)),
    upper = unname(stats::quantile(x, (1 + level) / 2))
  )
}
