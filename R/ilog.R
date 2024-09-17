#' Inverse Log Transformation
#'
#' Inverse log transforms a numeric atomic object.
#'
#' @details A wrapper on [`exp`]`(value)`.
#'
#' @inheritParams params
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
#' @inheritParams params
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
#' @inheritParams params
#' @family translations
#' @return A numeric atomic object.
#' @export
#' @examples
#' x <- c(2, 4.5)
#' ilog10(x)
ilog10 <- function(x) {
  exp10(x)
}
