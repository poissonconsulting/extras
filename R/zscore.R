#' Z-Score
#'
#' @param x A numeric vector of MCMC values.
#' @return A number.
#' @family {MCMC vector to scalar functions}
#' @export
#' @examples
#' zscore(as.numeric(0:100))
zscore <- function(x) {
  chk_numeric(x)
  mean(x)/stats::sd(x)
}
