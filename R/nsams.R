#' @importFrom universals nsams
#' @importFrom universals nterms
#' @export
universals::nsams

#' @inherit universals::nsams
#' @export
nsams.default <- function(x, ...) {
  nsims(x) * nterms(x)
}
