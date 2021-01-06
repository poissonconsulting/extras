#' Surprisal Value
#'
#' The surprisal value (Greenland 2019) is the [pvalue] expressed in
#' terms of how many consecutive heads would have to be thrown on a fair coin
#' in a single attempt to achieve the same probability.
#'
#'
#' @param x A numeric object of MCMC values.
#' @inheritParams params
#' @return A non-negative number.
#' @family summary
#' @references
#' Greenland, S. 2019. Valid P -Values Behave Exactly as They Should:
#' Some Misleading Criticisms of P -Values and Their Resolution With S -Values.
#' The American Statistician 73(sup1): 106–114.
#' <https://doi.org/10.1080/00031305.2018.1529625>.
#' @export
#' @examples
#' svalue(as.numeric(0:100))
svalue <- function(x, threshold = 0, na_rm = FALSE) {
  -log(pvalue(x, threshold = threshold, na_rm = na_rm), 2)
}
