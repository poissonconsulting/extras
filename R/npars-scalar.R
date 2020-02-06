#' @export
universals::npars_scalar

#' @inherit universals::npars_scalar
#' @export
npars_scalar.default <- function(x, ...) {
  chk_unused(...)
  x <- pars_scalar(x)
  if(anyNA(x)) return(NA_integer_)
  length(x)
}
