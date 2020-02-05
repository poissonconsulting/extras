#' Inverse Logistic Transformation
#'
#' Inverse logistically transforms a numeric atomic object.
#'
#' A wrapper on [`stats::plogis()`].
#'
#' @param x A numeric atomic object.
#' @return A numeric atomic object.
#' @family {translations}
#' @export
#' @examples
#' ilogit(c(-1, 0, 5))
ilogit <- function(x) {
  stats::plogis(x)
}

#' Log Transformation
#'
#' Replaces a object with the exponent of value.
#'
#' @details A wrapper on [`exp`]`(value)`.
#'
#' @param x An existing R object.
#' @param value A numeric atomic object.
#' @family {translations}
#' @export
#' @examples
#' x <- NULL
#' log(x) <- 0.5
#' x
`log<-` <- function(x, value) {
  exp(value)
}

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

#' Phi
#'
#' The standard normal cumulative density function.
#'
#' A wrapper on `stats::pnorm()`.
#'
#' @param x A numeric atomic object.
#' @return A numeric atomic object.
#' @family {translations}
#' @export
#' @examples
#' phi(0:2)
phi <- function(x) {
  stats::pnorm(x)
}

#' Power
#'
#' R equivalent to the C++ power function.
#'
#' Wrapper on `x^n`.
#'
#' @param x A numeric atomic object of the base.
#' @param n A numeric atomic object of the exponent.
#' @return A numeric atomic object of x raised to n.
#' @family {translations}
#' @export
#' @examples
#' pow(10, 2)
pow <- function(x, n) {
  x^n
}
