#' Bayesian Surprisal Value
#'
#' @param x A numeric vector of MCMC values.
#' @return A non-negative number.
#' @family {MCMC vector to scalar functions}
#' @export
#' @examples
#' svalue(as.numeric(0:100))
svalue <- function(x) {
  -log(pvalue(x), 2)
}
