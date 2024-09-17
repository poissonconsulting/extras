#' Log Transformation
#'
#' Replaces a object with the exponent of value.
#'
#' @details A wrapper on [`exp`]`(value)`.
#'
#' @inheritParams params
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

#' Log Base 2 Transformation
#'
#' Replaces a object with the base 2 exponent of value.
#'
#' @details A wrapper on [`exp2`]`(value)`.
#'
#' @inheritParams params
#' @param value A numeric atomic object.
#' @family translations
#' @return Called for the side effect of updating `x`.
#' @export
#' @examples
#' x <- NULL
#' log2(x) <- c(0.5, 5)
#' x
`log2<-` <- function(x, value) {
  exp2(value)
}

#' Log Base 10 Transformation
#'
#' Replaces a object with the base 10 exponent of value.
#'
#' @details A wrapper on [`exp10`]`(value)`.
#'
#' @inheritParams params
#' @param value A numeric atomic object.
#' @family translations
#' @return Called for the side effect of updating `x`.
#' @export
#' @examples
#' x <- NULL
#' log10(x) <- c(0.5, 5)
#' x
`log10<-` <- function(x, value) {
  exp10(value)
}
