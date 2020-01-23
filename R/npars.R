#' @importFrom universals npars
#' @importFrom universals pars
#' @export
universals::npars

#' @inherit universals::npars
#' @export
npars.default <- function(x, ...) {
  length(pars(x, ...))
}
