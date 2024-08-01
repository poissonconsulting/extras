#' Extreme Surprisal
#'
#' Calculates the surprisal (in bits) that a cumulative distribution function
#' probability is at least that extreme.
#' `r lifecycle::badge('deprecated')`
#'
#' @param x A numeric vector of values between 0 and 1.
#' @inheritParams params
#' @return A numeric vector of surprisal values.
#' @family residuals
#' @export
#'
#' @examples
#' sextreme(seq(0.1, 0.9, by = 0.1))
#' sextreme(seq(0.1, 0.9, by = 0.1), directional = TRUE)
sextreme <- function(x, directional = FALSE) {
  lifecycle::deprecate_soft("0.1.1", "sextreme()", id = "sextreme")
  chk_flag(directional)
  if (!length(x)) {
    return(numeric(0))
  }
  s <- -log(pextreme(x), 2)
  if (!directional) {
    return(s)
  }
  gt <- !is.na(x) & x > 0.5
  s[!gt] <- s[!gt] * -1
  s
}
