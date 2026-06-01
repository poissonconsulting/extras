#' Credible Intervals
#'
#' Calculates Bayesian credible intervals (CI) using one of the available methods:
#' - `"HDI"`: highest density interval (see [xtr_ci_hdi()]),
#' - `"ETI"`: equal tailed intervals (see [xtr_ci_eti()]).
#'
#' @inheritParams params
#' @param x A numeric vector of MCMC samples.
#' @param ... Currently unused.
#' @param type A string indicating which type of CI to return.
#' Currently allows Highest Density Intervals (`"HDI"`; default) and
#' Equal-Tailed Intervals (`"ETI"`).
#' @param na_rm A flag indicating whether to remove missing values.
#' @return A data frame of the `lower` and `upper` limits for the credible interval.
#' Note that the interval is not guaranteed to be one-sided or two-sided.
#' @export
#' @seealso [mcmcr::coef]
#' @examples
#' xtr_ci(rnorm(1e4), type = "HDI")
xtr_ci <- function(x, level = 0.95, ..., type = "HDI", na_rm = FALSE) {
  chk_string(type)
  chk_subset(type, c("HDI", "ETI"))
  chk_unused(...)

  if (type == "HDI") {
    return(xtr_ci_hdi(x = x, level = level, na_rm = na_rm))
  }
  xtr_ci_eti(x = x, level = level, na_rm = na_rm)
}
