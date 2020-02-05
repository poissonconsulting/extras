#' @export
universals::dims

#' @inherit universals::dims
#' @export
#' @examples
#' dims(character(0))
#' dims(1:3)
dims.default <- function(x, ...) {
  if (is.vector(x)) length(x) else dim(x)
}

#' @inherit universals::dims
#' @export
#' @examples
#' dims(factor("a"))
dims.factor <- function(x, ...) {
  length(x)
}
