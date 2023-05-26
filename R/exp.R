#' Exponential Transformation of Base 2
#'
#' Returns the transformation of 2^x.
#'
#' @param x An numeric atomic object.
#' @family translations
#' @return A numeric atomic object with the value of 2^x.
#' @export
#' @examples
#' x <- c(5, 10.5)
#' exp2(x)
exp2 <- function(x) {
  chk::chk_numeric(x)
  2^x
}

#' Exponential Transformation of Base 10
#'
#' Returns the transformation of 10^x.
#'
#' @param x An numeric atomic object.
#' @family translations
#' @return A numeric atomic object with the value of 10^x.
#' @export
#' @examples
#' x <- c(5, 10.5)
#' exp10(x)
exp10 <- function(x) {
  chk::chk_numeric(x)
  10^x
}
