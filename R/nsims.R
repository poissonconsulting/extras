#' @export
universals::nsims

#' @inherit universals::nsims
#' @export
nsims.default <- function(x, ...) {
  nchains(x) * niters(x)
}
