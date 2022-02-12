#' Percent Change
#'
#' Calculates the percent change for two sets of numbers.
#'
#' @param x A numeric object (vector, matrix or array) of non-negative numbers.
#' @param x2 A second numeric object of non-negative numbers.
#'
#' @return A numeric object of the percent change.
#' @family percent
#' @export
#'
#' @examples
#' percent_change(1, 2)
#' percent_change(2, 1)
percent_change <- function(x, x2) {
  proportional_change(x, x2) * 100
}

#' Percent Difference
#'
#' Calculates the percent difference for two sets of numbers.
#'
#' @param x A numeric object (vector, matrix or array) of non-negative numbers.
#' @param x2 A second numeric object of non-negative numbers.
#'
#' @return A numeric object of the percent change.
#' @family percent
#' @export
#'
#' @examples
#' percent_difference(1, 2)
#' percent_difference(2, 1)
percent_difference <- function(x, x2) {
  proportional_difference(x, x2) * 100
}

#' Percent Change2
#'
#' Calculates the percent change for a vector of two non-negative numbers.
#'
#' @param x A numeric vector of length 2.
#' @return A number.
#' @family percent, fun2
#' @export
#' @examples
#' percent_change2(c(1, 2))
#' percent_change2(c(2, 1))
percent_change2 <- function(x) {
  proportional_change2(x) * 100
}

#' Percent Difference2
#'
#' Calculates the percent difference for a vector of two non-negative numbers.
#'
#' @param x A numeric vector of length 2.
#' @return A number.
#' @family percent, fun2
#' @export
#' @examples
#' percent_difference2(c(1, 2))
#' percent_difference2(c(2, 1))
percent_difference2 <- function(x) {
  proportional_difference2(x) * 100
}
