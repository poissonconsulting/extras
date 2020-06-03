#' Phi
#'
#' The standard normal cumulative density function.
#'
#' A wrapper on `stats::pnorm()`.
#'
#' @param x A numeric atomic object.
#' @return A numeric atomic object.
#' @family translations
#' @export
#' @examples
#' phi(0:2)
phi <- function(x) {
  chk_natomic(x)
  stats::pnorm(x)
}
