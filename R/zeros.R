#' Zeros
#'
#' The number of zeros in an numeric object.
#'
#' @param x A numeric object of MCMC values.
#' @return A non-negative integer.
#' @family summary
#' @export
#' @examples
#' zeros(c(0:2))
zeros <- function(x) {
  chk_numeric(x)
  if(!length(x)) return(0L)
  sum(x == 0)
}
