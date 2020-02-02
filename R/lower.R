#' Lower Credible Limit
#'
#' @inheritParams params
#' @param x A numeric vector of MCMC values.
#' @return A number.
#' @family {MCMC vector to scalar functions}
#' @export
#' @examples
#' lower(as.numeric(0:100))
lower <- function(x, conf_level = 0.95) {
  chk_numeric(x)
  chk_number(conf_level)
  chk_range(conf_level)
  
  lower <- (1 - conf_level) / 2
  x <- stats::quantile(x, lower)
  x <- unname(x)
  x
}
