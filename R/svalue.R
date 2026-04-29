#' Surprisal Value
#'
#' The surprisal value (Greenland 2019) is the [pvalue] expressed in
#' terms of how many consecutive heads would have to be thrown on a fair coin
#' in a single attempt to achieve the same probability: \eqn{-\log_2(p)}, where
#' \eqn{p} is the p-value of interest.
#'
#' @param x A numeric object of MCMC values.
#' @inheritParams params
#' @param side A character indicating whether to calculate s-values using
#' p-values for the left tail (`"left"`), right tail (`"right"`), or both tails
#' (`"both"`).
#' Defaults to `NULL`, which is treated as `"both"` but returns a warning.
#' The warning will be removed in future package versions.
#' @return A non-negative number.
#' @family summary
#' @references
#' Greenland, S. 2019. Valid P-Values Behave Exactly as They Should:
#' Some Misleading Criticisms of P-Values and Their Resolution With S -Values.
#' The American Statistician 73(sup1): 106–114.
#' \doi{10.1080/00031305.2018.1529625}.
#' @export
#' @examples
#' svalue(rnorm(1e4, mean = 1), side = "left")
#' svalue(rnorm(1e4, mean = 1), side = "right")
svalue <- function(x, side = NULL, threshold = 0, na_rm = FALSE) {
  chk_numeric(x)
  chk_null_or(side, vld = vld_string)
  chk_null_or(side, vld = vld_subset, values = c("left", "right", "both"))
  chk_number(threshold)
  chk_logical(na_rm)

  if (is.null(side)) {
    rlang::warn("`side` should now be specified. Using `side = 'both'` by default. This warning will be removed in the future.")
    side <- "both"
  }

  -log2(pvalue(x, side = side, threshold = threshold, na_rm = na_rm))
}
