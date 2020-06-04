#' Lower Credible Limit
#'
#' Calculates the quantile-based lower credible limit.
#'
#' By default it returns the 95% credible limit which
#' corresponds to the 2.5% quantile.
#'
#' @inheritParams params
#' @param x A numeric vector of MCMC values.
#' @return A number.
#' @family summary
#' @export
#' @examples
#' lower(as.numeric(0:100))
lower <- function(x, conf_level = 0.95) {
  chk_numeric(x)
  chk_number(conf_level)
  chk_range(conf_level)

  if (!length(x)) {
    return(NA_real_)
  }
  if (anyNA(x)) {
    return(NA_real_)
  }

  lower <- (1 - conf_level) / 2
  x <- stats::quantile(x, lower)
  x <- unname(x)
  x
}
