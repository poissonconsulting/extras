#' Median
#'
#' @param x A numeric object of MCMC values.
#' @inheritParams params
#' @return A number.
#' @family summary
#' @export
#' @examples
#' xtr_mean(1:10)
xtr_median <- function(x, na_rm = FALSE) {
  chk_numeric(x)
  if(anyNA(x)) {
    if(isFALSE(na_rm)) return(NA_real_)
    x <- as.vector(x)
    x <- x[!is.na(x)]
  }
  if(!length(x)) return(NA_real_)
  stats::median(x)
}
