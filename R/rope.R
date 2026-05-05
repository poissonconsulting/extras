#' Region of Practical Equivalence
#'
#' Calculates the proportion of the posterior (or credible interval if
#' `ci_level < 1`) that falls within the region of practical equivalence (ROPE)
#' to a `threshold`.
#' The ROPE is calculated as `threshold + interval`.
#' Note that the default is not appropriate for all models, since the `interval`
#' is sensitive to unit conversions.
#'
#' @param x A numeric vector of MCMC samples.
#' @param ci_level A number > 0 and <= 1 specifying the probability coverage of
#' the HDI to use.
#' The default of 1 uses the full posterior.
#' @param quiet A flag indicating whether to return warnings.
#' @param na_rm A flag indicating whether to remove missing values.
#' @return A number indicating the estimated proportion of the posterior (or
#' credible interval) within the region of practical equivalence.
#' @export
#' @seealso [extras::xtr_hdi]
#' @examples
#' rope(c(-Inf, -1, -0.1, 0.1, 1, Inf))
#' rope(rnorm(1e4))
#' @name rope
NULL

rope <- function(x, threshold = 0, interval = c(-0.1, 0.1), ci_level = 1,
                 na_rm = FALSE, quiet = TRUE) {
  chk_numeric(x)
  chk_numeric(interval)
  chk_length(interval, 2)
  chk_number(ci_level)
  chk_true(all(ci_level > 0 & ci_level <= 1))
  chk_flag(na_rm)
  chk_flag(quiet)

  if (anyNA(x)) {
    if (vld_true(na_rm)) {
      x <- x[!is.na(x)]
    } else {
      NA_real_
    }
  }

  if(ci_level < 1) {
    ci <- xtr_hdi(x = x, level = ci_level, na_rm = na_rm, quiet = quiet)
    x <- x[x >= ci[1] & x <= ci[2]]
  }
  mean(x >= threshold + interval[1] & x <= threshold + interval[2])
}
