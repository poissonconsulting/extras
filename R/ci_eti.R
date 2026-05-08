#' Equal-Tailed Interval
#'
#' Calculates Bayesian credible intervals using the equal-tailed interval (ETI),
#' i.e., the CI such that the left and right tails outside the CI have the same
#' coverage. Note that the function does not return integer outputs, even if the
#' input data are integers, unlike [`xtr_ci_hdi()`].
#'
#' @param x A numeric vector of MCMC samples.
#' @param level A number > 0 and <= 1 specifying the probability coverage of the
#' ETI.
#' @param ... Currently unused.
#' @param na_rm A flag indicating whether to remove missing values.
#' @return A [data.frame] of the `lower` and `upper` limits for the credible
#' interval.
#' Note that the interval is guaranteed to be two-sided with real (i.e., double)
#' numeric limits, even if the input data are integers.
#' @export
#' @seealso [extras::xtr_ci()] and [extras::xtr_ci_hdi()]
#' @examples
#' xtr_ci_eti(rnorm(1e4))
#' @name xtr_ci_eti
NULL

xtr_ci_eti <- function(x, level = 0.95, ..., na_rm = FALSE) {
  chk_numeric(x)
  chk_number(level)
  chk_range(level, inclusive = TRUE)
  chk_gt(level)
  chk_unused(...)
  chk_flag(na_rm)

na <-  if(is.integer(x)) NA_integer_ else NA_real_

  if(length(x) <= 1) {
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
