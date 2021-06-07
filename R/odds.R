#' Odds
#'
#' Calculates the odds for probabilities.
#'
#' @param x A numeric object (vector, matrix or array) of probabilities.
#' @return A numeric object of the the odds for each probability.
#' @family translations
#' @seealso [odds_ratio()]
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
#' @seealso [odds()]
#' @export
#'
#' @examples
#' odds_ratio(0.5, 0.75)
odds_ratio <- function(x, x2) {
  odds(x) / odds(x2)
}
