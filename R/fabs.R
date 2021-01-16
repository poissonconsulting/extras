#' Absolute
#'
#' Computes the absolute value of `x`.
#' Used in TMB as replacement for `abs()` which is seemingly ambiguous.
#'
#' @details A wrapper on [`abs`]`()`.
#'
#' @param x An existing R object.
#' @family translations
#' @export
#' @examples
#' fabs(c(0, -1, 2))
fabs <- function(x) {
  abs(x)
}
