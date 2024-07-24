#' Proportion of Zeros
#'
#' The proportion of zeros in an numeric object.
#'
#' @param x A numeric object of MCMC values.
#' @inheritParams params
#' @return A number between 0 and 1.
#' @family summary
#' @export
#' @examples
#' pzeros(c(0:2))
pzeros <- function(x, na_rm = FALSE) {
  chk_numeric(x)
  if (anyNA(x)) {
    if (vld_false(na_rm)) {
      return(NA_real_)
    }
    x <- as.vector(x)
    x <- x[!is.na(x)]
  }
  if (!length(x)) {
    return(NaN)
  }
  sum(x == 0) / length(x)
}
