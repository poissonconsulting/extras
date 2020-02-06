#' @export
universals::npars

#' @inherit universals::npars
#' @export
npars.default <- function(x, ...) {
  chk_unused(...)
  x <- pars(x)
  if(anyNA(x)) return(NA_integer_)
  length(x)
}
