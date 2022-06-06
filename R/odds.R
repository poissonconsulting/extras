#' Odds
#'
#' Calculates the odds for probabilities.
#'
#' @param x A numeric object (vector, matrix or array) of probabilities.
#' @return A numeric object of the the odds for each probability.
#' @family odds
#' @export
#' @examples
#' odds(c(0, 0.5, 0.9, 1))
odds <- function(x) {
  chk_numeric(x)
  chk_range(x)
  x / (1 - x)
}

#' Odds Ratio
#'
#' Calculates the odds ratio for two probabilities.
#'
#' @param x A numeric object (vector, matrix or array) of probabilities.
#' @param x2 A second numeric object of probabilities.
#'
#' @return A numeric object of the odds ratios.
#' @family odds
#' @export
#'
#' @examples
#' odds_ratio(0.5, 0.75)
odds_ratio <- function(x, x2) {
  exp(log_odds_ratio(x, x2))
}

#' Log-Odds Ratio
#'
#' Calculates the log odds ratio for two probabilities.
#'
#' @param x A numeric object (vector, matrix or array) of probabilities.
#' @param x2 A second numeric object of probabilities.
#'
#' @return A numeric object of the log odds ratios.
#' @family odds
#' @export
#'
#' @examples
#' log_odds_ratio(0.5, 0.75)
log_odds_ratio <- function(x, x2) {
  log(odds(x)) - log(odds(x2))
}

#' Odds Ratio2
#'
#' Calculates the odds ratio for a vector of two probabilities.
#'
#' @param x A numeric vector of length 2.
#' @return A number.
#' @family odds fun2
#' @export
#' @examples
#' odds_ratio2(c(0.5,0.9))
#' odds_ratio2(c(0.9,0.5))
odds_ratio2 <- function(x) {
  chk_vector(x)
  chk_length(x, 2L)

  odds_ratio(x[1], x[2])
}

#' Log Odds Ratio2
#'
#' Calculates the log odds ratio for a vector of two probabilities.
#'
#' @param x A numeric vector of length 2.
#' @return A number.
#' @family odds fun2
#' @export
#' @examples
#' log_odds_ratio2(c(0.5,0.9))
#' log_odds_ratio2(c(0.9,0.5))
log_odds_ratio2 <- function(x) {
  chk_vector(x)
  chk_length(x, 2L)

  log_odds_ratio(x[1], x[2])
}

#' Inverse Odds
#'
#' Calculates the probabilities for odds.
#'
#' @param x A numeric object (vector, matrix or array) of odds.
#' @return A numeric object of the the probabilities for each odd.
#' @family odds
#' @export
#' @examples
#' inv_odds(c(0, 1, 9, 9999))
inv_odds <- function(x) {
  chk_numeric(x)
  chk_gte(x)
  x / (1 + x)
}

#' Inverse Odds Transformation
#'
#' Replaces an object with the inverse odds of value.
#'
#' @param x An existing R object.
#' @param value A numeric atomic object.
#' @family odds
#' @return Called for the side effect of updating `x`.
#' @export
#' @examples
#' x <- NULL
#' odds(x) <- 0.5
#' x
`odds<-` <- function(x, value) {
  chk_numeric(value)
  chk_gte(value)
  value / (1 + value)
}
