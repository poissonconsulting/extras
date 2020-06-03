#' Log Transformation
#'
#' Replaces a object with the exponent of value.
#'
#' @details A wrapper on [`exp`]`(value)`.
#'
#' @param x An existing R object.
#' @param value A numeric atomic object.
#' @family translations
#' @export
#' @examples
#' x <- NULL
#' log(x) <- 0.5
#' x
`log<-` <- function(x, value) {
  exp(value)
}
