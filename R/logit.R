#' Logistic Transformation
#'
#' Logistic transforms a numeric atomic object.
#'
#' A wrapper on [`stats::plogis()`].
#'
#' @param x A numeric atomic object.
#' @return The logistically transformed numeric atomic object.
#' @family {translations}
#' @export
#' @examples
#' logit(c(0.25, 0.5, 0.75))
logit <- function(x) {
  stats::qlogis(x)
}

#' Logistic Transformation
#'
#' @details A wrapper on `stats::plogis(value)`.
#'
#' @param x An existing object.
#' @param value A numeric atomic object of the value to inverse logistically transform.
#' @family {translations}
#' @export
#' @examples
#' x <- 1
#' logit(x) <- 0.5
#' x
`logit<-` <- function(x, value) {
  stats::plogis(value)
}
