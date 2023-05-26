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

#' Log Base 2 Transformation
#'
#' Replaces a object with the base 2 exponent of value.
#'
#' @details A wrapper on [`exp2`]`(value)`.
#'
#' @param x An existing R object.
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
#' @param x An existing R object.
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

#' Inverse Log Base 2 Transformation
#'
#' Inverse log transforms a numeric atomic object with base 2.
#'
#' @details A wrapper on [`exp2`]`(value)`.
#'
#' @param x A numeric atomic object.
#' @family translations
#' @return A numeric atomic object.
#' @export
#' @examples
#' x <- c(2, 4.5)
#' ilog2(x)

ilog2 <- function(x) {
  exp2(x)
}

#' Inverse Log Base 10 Transformation
#'
#' Inverse log transforms a numeric atomic object with base 10.
#'
#' @details A wrapper on [`exp10`]`(value)`.
#'
#' @param x A numeric atomic object.
#' @family translations
#' @return A numeric atomic object.
#' @export
#' @examples
#' x <- c(2, 4.5)
#' ilog10(x)

ilog10 <- function(x) {
  exp10(x)
}
