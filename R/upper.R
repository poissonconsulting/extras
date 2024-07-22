#' Upper Credible Limit
#'
#' Calculates the quantile-based upper credible limit.
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
upper <- function(x, conf_level = 0.95, na_rm = FALSE) {
  chk_numeric(x)
  chk_number(conf_level)
  chk_range(conf_level)

  if (anyNA(x)) {
    if (vld_false(na_rm)) {
      return(NA_real_)
    }
    x <- as.vector(x)
    x <- x[!is.na(x)]
  }
  if (!length(x)) {
    return(NA_real_)
  }

  lower <- (1 - conf_level) / 2
  upper <- conf_level + lower

  x <- stats::quantile(x, upper)
  x <- unname(x)
  x
}
