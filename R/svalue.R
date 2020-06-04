#' Surprisal Value
#'
#' The surprisal value is the probability of the [pvalue] expressed in
#' terms of how many consecutive heads would have to be thrown on a fair coin
#' in a single attempt to achieve the same probability.
#'
#'
#' @param x A numeric object of MCMC values.
#' @return A non-negative number.
#' @family summary
#' @references
#' Greenland, S. 2019. Valid P -Values Behave Exactly as They Should:
#' Some Misleading Criticisms of P -Values and Their Resolution With S -Values.
#' The American Statistician 73(sup1): 106–114.
#' <http://doi.org/10.1080/00031305.2018.1529625>.
#' @export
#' @examples
#' svalue(as.numeric(0:100))
svalue <- function(x) {
  -log(pvalue(x), 2)
}
