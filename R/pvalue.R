#' Bayesian P-Value
#'
#' @description {
#' A Bayesian p-value (p) is here defined in terms of the quantile-based
#' (1-p) * 100% credible interval (CRI) that
#' just includes a threshold (Kery and Schaub 2011).
#' By default a p-value of 0.05 indicates that the 95% CRI just includes the
#' threshold value.
#'
#' Note that the function contains the sample-size correction
#' \eqn{p_{c} = p * n / (n + 1)} to avoid p-values of 0. The function can still
#' return p-values of 1.
#'
#' To use as a measure of certainty in the direction of the estimate (i.e.,
#' positive or negative), see [`probability_direction()`].
#'
#' For p-values converted to bits, see [`svalue()`].
#'
#' To convert MCMC objects to information, see [`directional_information()`].
#' }
#'
#' @param x A numeric vector of MCMC values.
#' @param side A character vector of length 1 indicating whether to calculate
#' p-values for the left tail (`"left"`), right tail (`"right"`), or two-sided (`"both"`; default).
#' @inheritParams params
#' @return A number between 0 and 1.
#' @family summary
#' @references
#' Kery, M., and Schaub, M. 2011.
#' Bayesian population analysis using WinBUGS: a hierarchical perspective.
#' Academic Press, Boston. Available from <https://www.vogelwarte.ch/en/research/population-biology/book-bpa/>.
#' @export
#' @examples
#' x <- rnorm(1e6, qnorm(0.05, lower.tail = TRUE))
#' pvalue(x) # should be 0.05 * 2
#' pvalue(x, side = "left") # should be 0.95
#' pvalue(x, side = "right") # should be 0.05
pvalue <- function(x, side = "both", threshold = 0, na_rm = FALSE) {
  chk_numeric(x)
  chk_string(side)
  chk_subset(side, c("left", "right", "both"))
  chk_number(threshold)
  chk_logical(na_rm)

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

  if (side == "both") {
    s1 <- sum(x < threshold)
    s2 <- sum(x > threshold)
    s <- min(s1, s2)
    s <- s * 2 # two sided p-value
    s <- s + n - s1 - s2 # include threshold samples
  } else if (side == "left") {
    s <- sum(x <= threshold) # include threshold samples
  } else if (side == "right") {
    s <- sum(x >= threshold) # include threshold samples
  }
  p <- s / n
  p <- max(p, 1 / (n + 1)) # avoid pvalues of 0
  p
}
