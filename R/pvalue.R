#' Bayesian P-Value
#'
#' A Bayesian p-value (p) is here defined in terms of the quantile-based
#' (1-p) * 100% credible interval (CRI) that
#' just includes 0 (Kery and Schaub 2011).
#' In other words a p-value of 0.05 indicates that the 95% CRI just includes 0.
#'
#' @param x A numeric vector of MCMC values.
#' @return A number between 0 and 1.
#' @family summary
#' @references
#' Kery, M., and Schaub, M. 2011.
#' Bayesian population analysis using WinBUGS: a hierarchical perspective.
#' Academic Press, Boston. Available from <https://www.vogelwarte.ch/de/projekte/publikationen/bpa/>.
#' @export
#' @examples
#' pvalue(as.numeric(0:100))
pvalue <- function(x) {
  chk_numeric(x)
  if (!length(x)) {
    return(NA_real_)
  }
  n <- length(x)
  s1 <- sum(x < 0)
  s2 <- sum(x > 0)
  s <- min(s1, s2)
  s <- s * 2 # two sided p-value
  s <- s + n - s1 - s2 # include 0 values
  (s + 1) / (n + 1) # avoid pvalues of 0
}
