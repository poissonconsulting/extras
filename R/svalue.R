#' Surprisal Value
#'
#' @description
#' The [surprisal value](https://www.poissonconsulting.ca/post/2026/what-are-s-values/)
#' (Greenland 2019) is a probability expressed in terms of how many consecutive
#' heads would have to be thrown on a fair coin in a single attempt to achieve
#' the same probability: \eqn{-\log_2(p)}, where \eqn{p} is the p-value of
#' interest. See the details section for some examples.
#'
#' @details {
#' A near-certain event has an s-value near 0 because it is similar to getting
#' 0 successful coin flips out of 0 tosses, which is certain and unsurprising.
#'
#' An event with a probability of 0.5 is as surprising as getting a successful
#' coin toss.
#'
#' A near-impossible event has a very large s-value because its
#' occurrence would be extremely surprising, like observing many consecutive
#' successes on a fair coin.
#'
#' When `skeptical = TRUE`, a floor of \eqn{1 / (n + 1)} is applied to the
#' underlying p-value to avoid s-values of `Inf` when all samples are on one
#' side of the threshold. When `skeptical = FALSE`, s-values of `Inf` are
#' allowed. The default will change from `TRUE` to `FALSE` in a future release.
#' }
#'
#' @describeIn svalue Calculate an s-value from a posterior distribution.
#' @param x A numeric object of MCMC values.
#' @param ... Unused.
#' @inheritParams params
#' @param side A character indicating whether to calculate s-values using
#' p-values for the left tail (`"left"`), right tail (`"right"`), or both tails
#' (`"both"`; default).
#' @return A non-negative number.
#' If `x` has `NA` values but `na_rm` is `FALSE`, returns `NA_real`.
#' @family summary
#' @references
#' Greenland, S. 2019. Valid P-Values Behave Exactly as They Should:
#' Some Misleading Criticisms of P-Values and Their Resolution With S-Values.
#' The American Statistician 73(sup1): 106–114.
#' \doi{10.1080/00031305.2018.1529625}.
#' @export
#' @examples
#' svalue(as.numeric(0:100), skeptical = TRUE)
#' svalue(as.numeric(0:100), side = "left", skeptical = TRUE)
#' svalue(as.numeric(0:100), side = "right", skeptical = TRUE)
#' svalue(rnorm(1e4, mean = 1), side = "left", skeptical = TRUE)
#' svalue(rnorm(1e4, mean = 1), side = "right", skeptical = TRUE)
#' svalue(rep(1, 10), skeptical = TRUE) # skeptical = TRUE avoids Inf
#' svalue(rep(1, 10), skeptical = FALSE) # skeptical = FALSE allows Inf
#'
#' p2svalue(seq(0, 1, by = 0.1))
svalue <- function(x, ..., side = "both", threshold = 0, skeptical = TRUE, na_rm = FALSE) {
  chk_unused(...)
  chk_logical(skeptical)
  chk_numeric(x)
  chk_string(side)
  chk_subset(side, values = c("left", "right", "both"))
  chk_number(threshold)
  chk_flag(na_rm)

  if (missing(skeptical)) {
    lifecycle::deprecate_soft(
      when = "0.10.0",
      what = "svalue(skeptical)",
      details = "The default will change to `skeptical = FALSE`."
    )
  }

  -log2(pvalue(x, side = side, threshold = threshold, skeptical = skeptical, na_rm = na_rm))
}

#' @describeIn svalue Calculate an s-value from a vector of probabilities.
#' @export
p2svalue <- function(p) {
  chk_numeric(p)
  chk_range(p)
  -log2(p)
}
