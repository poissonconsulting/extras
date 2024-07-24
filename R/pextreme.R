#' Extreme Probability
#'
#' Calculates the probability that a cumulative distribution function
#' probability is at least that extreme.
#' `r lifecycle::badge('deprecated')`
#'
#' @param x A numeric vector of values between 0 and 1.
#' @return A numeric vector of values between 0 and 1.
#' @family residuals
#' @export
#'
#' @examples
#' pextreme(seq(0, 1, by = 0.1))
pextreme <- function(x) {
  lifecycle::deprecate_soft("0.1.1", "pextreme()", id = "sextreme")
  chk_numeric(x)
  chk_range(x)

  if (!length(x)) {
    return(numeric(0))
  }
  gt <- !is.na(x) & x > 0.5
  x[gt] <- 1 - x[gt]
  x[!is.na(x)] <- x[!is.na(x)] * 2
  x
}
