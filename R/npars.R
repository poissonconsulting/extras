#' @importFrom universals pars
#' @importFrom universals npars
#' @export
universals::npars

#' Number of Parameters of an Object
#' 
#' @inherit universals::npars
#' @export
npars.default <- function(x, ...) {
  length(pars(x, ...))
}
