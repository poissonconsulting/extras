#' @export
universals::npars

#' @inherit universals::npars
#' @export
npars.default <- function(x, ...) {
  x <- pars(x, ...)
  if (anyNA(x)) {
    return(NA_integer_)
  }
  length(x)
}
