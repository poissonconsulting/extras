#' Upper Credible Limit
#'
#' @inheritParams params
#' @param x A numeric vector of MCMC values.
#' @return A number.
#' @family {MCMC vector to scalar functions}
#' @export
#' @examples
#' upper(as.numeric(0:100))
upper <- function(x, conf_level = 0.95) {
  chk_numeric(x)
  chk_number(conf_level)
  chk_range(conf_level)
  
  lower <- (1 - conf_level) / 2
  upper <- conf_level + lower

  x <- stats::quantile(x, upper)
  x <- unname(x)
  x
}
