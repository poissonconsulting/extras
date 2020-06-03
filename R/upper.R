#' Upper Credible Limit
#'
#' Calculates the upper credible limit for a
#' vector of MCMC values.
#'
#' By default it returns the 95% credible limit which
#' corresponds to the 97.5% quantile.
#'
#' @inheritParams params
#' @param x A numeric vector of MCMC values.
#' @return A number.
#' @family summary
#' @export
#' @examples
#' upper(as.numeric(0:100))
upper <- function(x, conf_level = 0.95) {
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
  upper <- conf_level + lower

  x <- stats::quantile(x, upper)
  x <- unname(x)
  x
}
