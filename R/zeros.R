#' Zeros
#'
#' The number of zeros in an numeric object.
#'
#' @param x A numeric object of MCMC values.
#' @inheritParams params
#' @return A non-negative integer.
#' @family summary
#' @export
#' @examples
#' zeros(c(0:2))
zeros <- function(x, na_rm = FALSE) {
  chk_numeric(x)
  if (anyNA(x)) {
    if (vld_false(na_rm)) {
      return(NA_integer_)
    }
    x <- as.vector(x)
    x <- x[!is.na(x)]
  }
  if (!length(x)) {
    return(0L)
  }
  sum(x == 0)
}
