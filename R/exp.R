#' Exponential transformation of 2^x
#'
#' Inverse of log2(x)
#'
#' @details Returns the value of 2^x
#'
#' @param x An numeric atomic object.
#' @family translations
#' @return A numeric atomic object.
#' @export
#' @examples
#' x <- 5
#' exp2(x) # 32
#' 2^x # 32
exp2 <- function(x) {
  bol <- !is.na(x)
  if (is.null(x)) bol[is.null(x)] <- TRUE
  if (any(bol)) chk::chk_numeric(x[bol])
  2^x
}

#' Exponential transformation of 10^x
#'
#' Inverse of log10(x)
#'
#' @details Returns the value of 10^x
#'
#' @param x An numeric atomic object.
#' @family translations
#' @return A numeric atomic object.
#' @export
#' @examples
#' x <- 5
#' exp10(x) # 1e+05
#' 10^x # 1e+05
exp10 <- function(x) {
  bol <- !is.na(x)
  if (is.null(x)) bol[is.null(x)] <- TRUE
  if (any(bol)) chk::chk_numeric(x[bol])
  10^x
}
