#' Step
#'
#' @param x A numeric atomic object.
#' @family translations
#' @return A logical value.
#' @export
#'
#' @examples
#' step(1)

step <- function(x) {
  chk_numeric(x)
  x >= 0
}
