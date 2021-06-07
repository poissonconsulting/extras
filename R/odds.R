#' Odds
#'
#' Calculates the odds for probabilities.
#'
#' @param x A numeric object (vector, matrix or array) of probabilities.
#' @return A numeric object of the the odds for each probability.
#' @family translations
#' @export
#' @examples
#' odds(c(0, 0.5, 0.9, 1))
odds <- function(x) {
  chk_numeric(x)
  chk_range(x)
  x / (1 - x)
}
