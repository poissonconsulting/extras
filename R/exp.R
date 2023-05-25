#' Exponential transformation of base 2
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
  bol <- !is.na(x)
  if (is.null(x)) bol[is.null(x)] <- TRUE
  if (any(bol)) chk::chk_numeric(x[bol])
  2^x
}

#' Exponential transformation of base 10
#'
#' Returns the transformation of 10^x.
#'
#' @param x An numeric atomic object.
#' @family translations
#' @return A numeric atomic object.
#' @export
#' @examples
#' x <- c(5, 10.5)
#' exp10(x)
exp10 <- function(x) {
  bol <- !is.na(x)
  if (is.null(x)) bol[is.null(x)] <- TRUE
  if (any(bol)) chk::chk_numeric(x[bol])
  10^x
}
