#' Power
#'
#' R equivalent to the C++ power function.
#'
#' Wrapper on `x^n`.
#'
#' @param x A numeric atomic object of the base.
#' @param n A numeric atomic object of the exponent.
#' @return A numeric atomic object of x raised to n.
#' @family {translations}
#' @export
#' @examples
#' pow(10, 2)
pow <- function(x, n) {
  x^n
}
