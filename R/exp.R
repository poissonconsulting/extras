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
#' exp2(x)
#' 2^x
exp2 <- function(x) {
  if (is.null(x) | any(!is.numeric(x) & !is.na(x))) stop("non-numeric argument to mathematical function")
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
#' exp10(x)
#' 10^x
exp10 <- function(x) {
  if (is.null(x) | any(!is.numeric(x) & !is.na(x))) stop("non-numeric argument to mathematical function")
  10^x
}
