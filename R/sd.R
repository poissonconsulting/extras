#' Standard Deviation
#'
#' @param x A numeric object of MCMC values.
#' @inheritParams params
#' @return A number.
#' @family summary
#' @export
#' @examples
#' xtr_sd(1:10)
xtr_sd <- function(x, na_rm = FALSE) {
  chk_numeric(x)
  if(anyNA(x)) {
    if(vld_false(na_rm)) return(NA_real_)
    x <- as.vector(x)
    x <- x[!is.na(x)]
  }
  if(!length(x)) return(NA_real_)
  stats::sd(x)
}
