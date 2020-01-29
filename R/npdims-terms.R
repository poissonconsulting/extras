#' @export
universals::npdims_terms

#' @inherit universals::npdims_terms
#' @export
npdims_terms.default <- function(x, ...) {
  vapply(pdims_terms(x, ...), length, 1L)
}
