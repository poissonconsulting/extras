#' Z-Score
#'
#' @param x A numeric object of MCMC values.
#' @return A number.
#' @family summary
#' @export
#' @examples
#' zscore(as.numeric(0:100))
zscore <- function(x) {
  chk_numeric(x)
  if (!length(x)) {
    return(NA_real_)
  }
  x <- mean(x) / stats::sd(x)
  if (is.nan(x)) {
    return(NA_real_)
  }
  x
}
