#' Zeros
#'
#' The number of zeros in an numeric object.
#'
#' @inheritParams params
#' @param x A numeric object of MCMC values.
#' @return A non-negative integer.
#' @family summary
#' @export
#' @examples
#' zeros(c(0:2))
zeros <- function(x, na_rm = FALSE) {
  if(!length(x)) return(0L)
  if(anyNA(x)) {
    if(isFALSE(na_rm)) {
      return(NA_integer_)
    }
    x <- fill_na(x, 1L)
  }
  sum(x == 0)
}
