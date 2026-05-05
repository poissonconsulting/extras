#' Equal-Tailed Interval
#'
#' Calculates Bayesian credible intervals using the equal-tailed interval (ETI),
#' i.e., the CI such that the left and right tails outside the CI have the same
#' coverage.
#'
#' @param x A numeric vector of MCMC samples.
#' @param level A number between 0 and 1 (exclusive) specifying the probability
#' coverage of the ETI.
#' @param quiet A flag indicating whether to return warnings.
#' @param na_rm A flag indicating whether to remove missing values.
#' @return A data frame of the `lower` and `upper` limits for the credible interval.
#' Note that the interval is not guaranteed to be one-sided or two-sided.
#' @export
#' @seealso [extras::xtr_ci], [extras::xtr_ci_hdi], and [extras::xtr_ci_norm]
#' @examples
#' xtr_ci_eti(rnorm(1e4))
#' @name xtr_ci_eti
NULL

xtr_ci_eti <- function(x, level = 0.95, na_rm = FALSE, quiet = TRUE) {
  chk_numeric(x)
  chk_number(level)
  chk_range(level)
  chk_flag(na_rm)
  chk_flag(quiet)

  if(length(x) == 0) {
    return(data.frame(lower = NA_real_, upper = NA_real_))
  }

  if (anyNA(x)) {
    if (vld_true(na_rm)) {
      x <- x[!is.na(x)]
    } else {
      return(data.frame(lower = NA_real_, upper = NA_real_))
    }
  }

  data.frame(
    lower = unname(quantile(x, (1 - level) / 2)),
    upper = unname(quantile(x, (1 + level) / 2))
  )
}
