#' Probability of Direction
#'
#' The probability of direction (PD) is the proportion of the (posterior)
#' distribution above or below zero.
#' By default, the direction is based on the side of the median value, but it
#' can be specified to measure support for specific hypotheses.
#' A right-side PD of 0.9 indicates that the CRI spanning from the threshold to
#' infinity has a coverage of 90%.
#' Can be used as a measure of certainty in the direction of the estimate
#' (e.g., positive or negative when using a threshold of 0).
#' **NOTE:** probability estimates of 0 or 1 are corrected towards 0.5 by adding
#' or subtracting `1 / (length(x) + 1)`, where `x` is a vector of MCMC samples.
#' Ideally, `x` should be large enough as to make the correction negligible.
#'
#' @param x A numeric vector of MCMC values.
#' @param side A character vector of length 1 indicating whether to calculate
#' the directional probability for the left tail (`"left"`; `x < threshold`),
#' or the right tail (`"right"`; `x > threshold`).
#' Defaults to `NULL`, which uses the side of the median of `x` via
#' [`direction()`].
#' @param threshold A number of the threshold value, which is excluded from the
#' interval for the probability.
#' @inheritParams params
#' @return A number between 0 and 1.
#' @family summary
#' @references
#' Makowski, D., Ben-Shachar, M.S., Chen, S.H.A., and Lüdecke, D. 2019. Indices of Effect Existence and Significance in the Bayesian Framework. Front. Psychol. 10: 2767. \doi{10.3389/fpsyg.2019.02767}.
#' @export
#' @examples
#' x <- rnorm(1e6, qnorm(0.05, lower.tail = TRUE))
#' probability_direction(x, side = "left")
#' probability_direction(x, side = "right") # = 1 - probability_direction(x, side = "left")
#' probability_direction(c(0, 0, 1), side = "right") # does not include threshold
#' probability_direction(c(1, 1), side = "right") # p = 1 - 1/(n+1)
probability_direction <- function(x, side = NULL, threshold = 0, na_rm = FALSE) {
  chk_numeric(x)
  chk_null_or(side, vld = vld_string)
  chk_null_or(side, vld = vld_subset, values = c("left", "right"))
  chk_number(threshold)
  chk_flag(na_rm)

  if (anyNA(x)) {
    if (na_rm) {
      x <- as.vector(x)
      x <- x[!is.na(x)]
    } else {
      return(NA_real_)
    }
  }

  n <- length(x)
  if (n == 0) {
    return(NA_real_)
  }

  if (side == "median") {
    side <- direction(x)
  }

  if (side == "left") {
    s <- sum(x < threshold) # exclude threshold samples
  } else if (side == "right") {
    s <- sum(x > threshold) # exclude threshold samples
  }

  p <- s / n

  if (p == 0) {
    p <- 1 / (n + 1)
  }
  if (p == 1) {
    p <- s / (n + 1)
  }

  p
}
