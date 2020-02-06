#' @export
universals::npars_scalar

#' @inherit universals::npars_scalar
#' @export
npars_scalar.default <- function(x, ...) {
  length(pars_scalar(x, ...))
}
