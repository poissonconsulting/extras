#' Bayesian P-Value
#'
#' A Bayesian p-value (p) is here defined in terms of the quantile-based
#' (1-p) * 100% credible interval (CRI) that
#' just includes a threshold (Kery and Schaub 2011).
#' By default a p-value of 0.05 indicates that the 95% CRI just includes 0.
#'
#' @param x A numeric vector of MCMC values.
#' @inheritParams params
#' @param two_sided logical of length 1: calculate two-sided p-values if `TRUE` (default) and one-sided if `FALSE`. See `side`.
#' @param side character of length 1: whether to calculate p-values for the left tail (`"left"`) or right tail (`"right"`).
#' @return A number between 0 and 1.
#' @family summary
#' @references
#' Kery, M., and Schaub, M. 2011.
#' Bayesian population analysis using WinBUGS: a hierarchical perspective.
#' Academic Press, Boston. Available from <https://www.vogelwarte.ch/en/research/population-biology/book-bpa/>.
#' @export
#' @examples
#' x <- rnorm(1e6, qnorm(0.05))
#' hist(x); abline(v = 0, col = "red", lwd = 2, lty = 2)
#' pvalue(x) # should be 0.05 * 2
#' pvalue(x, two_sided = FALSE, side = "left") # should be 0.05
#' pvalue(x, two_sided = FALSE, side = "right") # should be 0.95
pvalue <- function(x, threshold = 0, two_sided = TRUE, side = "", na_rm = FALSE) {
  chk_numeric(x)
  chk_number(threshold)
  chk_logical(two_sided)
  chk_null_or(side, vld = vld_character)
  if (two_sided) {
    if (nchar(side) > 0) {
      warning("`side` is ignored if `two_sided = TRUE`.")
    }
  } else {
    if (! side %in% c("left", "right")) {
      stop("`side` must be \"left\" or \"right\" if `two_sided = FALSE`.")
    }
  }

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

  if (two_sided) {
    n <- length(x)
    s1 <- sum(x < threshold)
    s2 <- sum(x > threshold)
    s <- min(s1, s2)
    s <- s * 2 # two sided p-value
    s <- s + n - s1 - s2 # include threshold values
    p <- (s + 1) / (n + 1) # avoid pvalues of 0
    return(p)
  } else {
    if (side == "left") {
      s <- sum(x <= threshold) # include threshold values
    } else {
      s <- sum(x >= threshold) # include threshold values
    }
    n <- length(x)
    p <- (s + 1) / (n + 1) # avoid pvalues of 0
    return(p)
  }
}
