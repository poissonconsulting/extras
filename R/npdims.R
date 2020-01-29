#' @export
universals::npdims

#' @inherit universals::npdims
#' @export
npdims.default <- function(x, ...) {
  vapply(pdims(x, ...), length, 1L)
}
