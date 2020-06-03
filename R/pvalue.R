#' Bayesian P-Value
#'
#' @param x A numeric vector of MCMC values.
#' @return A number between 0 and 1.
#' @family summary
#' @export
#' @examples
#' pvalue(as.numeric(0:100))
pvalue <- function(x) {
  chk_numeric(x)
  if (!length(x)) {
    return(NA_real_)
  }
  n <- length(x)
  s <- min(sum(x < 0), sum(x > 0))
  s <- s * 2
  s <- s + sum(x == 0)
  (s + 1) / (n + 1)
}
