#' Z-Score
#'
#' The Bayesian z-score is here defined as the number of standard deviations
#' from the mean estimate to zero.
#'
#' @param x A numeric object of MCMC values.
#' @inheritParams params
#' @return A number.
#' @family summary
#' @export
#' @examples
#' zscore(as.numeric(0:100))
zscore <- function(x, na_rm = FALSE) {
  chk_numeric(x)

  if (anyNA(x)) {
    if (vld_false(na_rm)) {
      return(NA_real_)
    }
    x <- as.vector(x)
    x <- x[!is.na(x)]
  }
  if (length(x) < 2) {
    return(NA_real_)
  }
  mean(x) / stats::sd(x)
}
