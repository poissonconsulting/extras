#' Bayesian Surprisal Value
#'
#'
#'
#' @param x A numeric vector of MCMC values.
#' @return A non-negative number.
#' @family summary
#' @export
#' @examples
#' svalue(as.numeric(0:100))
svalue <- function(x) {
  -log(pvalue(x), 2)
}
