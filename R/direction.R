#' Direction of a distribution
#'
#' The direction of a distribution is the side (left/right) that the
#' distribution's center falls on, relative to a threshold. The center can be
#' calculated using a user-specified function, including the `[median]` (default),
#' `[mean]`, geometric mean, mode, or any other custom function. By convention,
#' values below the threshold fall to the left, while values above the threshold
#' fall to the right. Center estimates equal to the threshold are assumed to
#' fall to the right.
#'
#' @param x A numeric vector of MCMC values or any other numeric vector of samples.
#' @param estimate A function for estimating the center of the distribution.
#' Defaults to [`median()`], but can also be `[mean()`] or any custom function
#' that returns a number (non-missing numeric vector of length 1).
#' `NA` values are dropped before calling the function.
#' @param threshold A number of the threshold value.
#' @inheritParams params
#' @return A character vector of length one indicating if at least half of the
#' observations are above the threshold (`"right"`) or not (`"left"`).
#' @family summary
#' @export
#' @examples
#' direction(c(1, 2, 3))
#' direction(c(-1))
#' direction(c(0, 0, 0))
#' direction(c(-100, 1, 1))
#' direction(c(-100, 1, 1), mean)
#' direction(c(100, 0.01, 0.01), function(.x) exp(mean(log(.x))))
direction <- function(x, estimate = median, threshold = 0, na_rm = FALSE) {
  chk_numeric(x)
  chk_number(threshold)
  chk_logical(na_rm)
  chk_function(estimate)

  if (na_rm) {
    x <- x[! is.na(x)]
  }

  if (length(x) == 0) {
    return(NA_character_)
  }

  est <- estimate(x)
  if(!vld_number(est)) {
    err("The estimate function must return a number (non-missing numeric scalar).")
  }

  if (est < threshold) {
    return("left")
  }
  "right"
}
