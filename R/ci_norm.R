#' Normal Approximation Intervals
#'
#' Calculates equal-tailed credible interval using using a normal (i.e.,
#' Gaussian) approximation of the distribution.
#' The intervals may be similar to those produced by `xtr_ci_eti()` when the
#' distributions of the data are symmetric.
#' Returns `NA_real_` if any elements of `x` are infinite.
#'
#' @param x A numeric vector of MCMC samples.
#' @param level A number between 0 and 1 (exclusive) specifying the probability
#' coverage of the interval.
#' @param quiet A flag indicating whether to return warnings.
#' @param na_rm A flag indicating whether to remove missing values.
#' @return A data frame of the `lower` and `upper` limits for the credible interval.
#' Note that the interval is not guaranteed to be one-sided or two-sided.
#' @export
#' @seealso [extras::xtr_ci()], [extras::xtr_ci_eti()], and [extras::xtr_ci_hdi()]
#' @examples
#' xtr_ci_norm(rnorm(1e4))
#' @name xtr_ci_norm
NULL

xtr_ci_norm <- function(x, level = 0.95, na_rm = FALSE, quiet = TRUE) {
  chk_numeric(x)
  chk_number(level)
  chk_range(level)
  chk_flag(na_rm)
  chk_flag(quiet)

  # if(length(x) == 0) {
  #   return(data.frame(lower = NA_real_, upper = NA_real_))
  # }
  if (any(is.infinite(x))) {
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
    lower = xtr_mean(x) + stats::qnorm((1 - level) / 2) * xtr_sd(x),
    upper = xtr_mean(x) + stats::qnorm((1 + level) / 2) * xtr_sd(x)
  )
}
