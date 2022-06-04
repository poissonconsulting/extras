#' Log Transformation
#'
#' Replaces a object with the exponent of value.
#'
#' @details A wrapper on [`exp`]`(value)`.
#'
#' @param x An existing R object.
#' @param value A numeric atomic object.
#' @family translations
#' @return Called for the side effect of updating `x`.
#' @export
#' @examples
#' x <- NULL
#' log(x) <- 0.5
#' x
`log<-` <- function(x, value) {
  exp(value)
}

#' Inverse Log Transformation
#'
#' Inverse log transforms a numeric atomic object.
#'
#' @details A wrapper on [`exp`]`(value)`.
#'
#' @param x A numeric atomic object.
#' @family translations
#' @return A numeric atomic object.
#' @export
#' @examples
#' x <- 1
#' ilog(x)

ilog <- function(x) {
  exp(x)
}
