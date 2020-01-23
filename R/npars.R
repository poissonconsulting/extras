#' @importFrom universals npars
#' @importFrom universals pars
#' @export
universals::npars

#' Number of Parameters of an Object
#' 
#' @inherit universals::npars
#' @export
npars.default <- function(x, ...) {
  length(pars(x, ...))
}
