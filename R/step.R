#' Step
#'
#' @param x A numeric atomic object.
#' @return A logical value.
#' @export
#'
#' @examples
#' step(1)

step <- function(x) {
  chk_numeric(x)
  x >= 0
}
