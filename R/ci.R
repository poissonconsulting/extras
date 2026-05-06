#' Credible Intervals
#'
#' Calculates Bayesian credible intervals (CI) using one of the available methods:
#' - `"HDI"`: highest density interval (see [xtr_ci_hdi()]),
#' - `"ETI"`: equal tailed intervals (see [xtr_ci_eti()]),
#' - `"normal"`: normal approximation intervals (see [xtr_ci_norm()]).
#'
#' @param x A numeric vector of MCMC samples.
#' @param level A number between 0 and 1 (exclusive) specifying the probability
#' coverage of the CI.
#' @param quiet A flag indicating whether to return warnings.
#' @param na_rm A flag indicating whether to remove missing values.
#' @return A data frame of the `lower` and `upper` limits for the credible interval.
#' Note that the interval is not guaranteed to be one-sided or two-sided.
#' @export
#' @seealso [mcmcr::coef]
#' @examples
#' xtr_ci(rnorm(1e4), type = "HDI")
#' @name xtr_ci
NULL

xtr_ci <- function(x, level = 0.95, type = "HDI", quiet = TRUE, na_rm = FALSE) {
  chk_numeric(x)
  chk_number(level)
  chk_subset(type, c("HDI", "ETI"))
  chk_range(level)
  chk_flag(na_rm)
  chk_flag(quiet)

  if(type == "HDI") {
    xtr_ci_hdi(x = x, level = level, na_rm = na_rm, quiet = quiet)
  } else if (type == "ETI") {
    xtr_ci_eti(x = x, level = level, na_rm = na_rm)
  }
}
