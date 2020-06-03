#' Inverse Logistic Transformation
#'
#' Inverse logistically transforms a numeric atomic object.
#'
#' A wrapper on [`stats::plogis()`].
#'
#' @param x A numeric atomic object.
#' @return A numeric atomic object.
#' @family translations
#' @export
#' @examples
#' ilogit(c(-1, 0, 5))
ilogit <- function(x) {
  stats::plogis(x)
}
