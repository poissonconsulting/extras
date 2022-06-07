#' Proportional Change
#'
#' Calculates the proportional change for two sets of numbers.
#'
#' @param x A numeric object (vector, matrix or array) of non-negative numbers.
#' @param x2 A second numeric object of non-negative numbers.
#'
#' @return A numeric object of the proportional change.
#' @family proportional
#' @export
#'
#' @examples
#' proportional_change(1, 2)
#' proportional_change(2, 1)
proportional_change <- function(x, x2) {
  chk_numeric(x)
  chk_gte(x)

  chk_numeric(x2)
  chk_gte(x2)

  (x2 - x) / x
}

#' Proportional Difference
#'
#' Calculates the proportional difference for two sets of numbers.
#'
#' @param x A numeric object (vector, matrix or array) of non-negative numbers.
#' @param x2 A second numeric object of non-negative numbers.
#'
#' @return A numeric object of the proportional change.
#' @family proportional
#' @export
#'
#' @examples
#' proportional_difference(1, 2)
#' proportional_difference(2, 1)
proportional_difference <- function(x, x2) {
  chk_numeric(x)
  chk_gte(x)

  chk_numeric(x2)
  chk_gte(x2)

  (x2 - x) / (x + x2) * 2
}

#' Proportional Change2
#'
#' Calculates the proportional change for a vector of two non-negative numbers.
#'
#' @param x A numeric vector of length 2.
#' @return A number.
#' @family proportional fun2
#' @export
#' @examples
#' proportional_change2(c(1, 2))
#' proportional_change2(c(2, 1))
proportional_change2 <- function(x) {
  chk_vector(x)
  chk_length(x, 2L)

  proportional_change(x[1], x[2])
}

#' Proportional Difference2
#'
#' Calculates the proportional difference for a vector of two non-negative numbers.
#'
#' @param x A numeric vector of length 2.
#' @return A number.
#' @family proportional fun2
#' @export
#' @examples
#' proportional_difference2(c(1, 2))
#' proportional_difference2(c(2, 1))
proportional_difference2 <- function(x) {
  chk_vector(x)
  chk_length(x, 2L)

  proportional_difference(x[1], x[2])
}
