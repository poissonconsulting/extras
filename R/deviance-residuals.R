#' Poisson Deviance Residual
#'
#' @param x A non-negative whole numeric vector of values.
#' @param lambda A non-negative numeric vector of means.
#'
#' @return An numeric vector of the corresponding deviance residuals.
#' @export
#'
#' @examples
#' devpois(c(1,3.5,4), 3)
devpois <- function(x, lambda) {
  dev <- x * log(x/lambda) - (x - lambda)
  dev <- pmax(dev * 2, 0)
  dev <- sqrt(dev)
  dev * sign(x - lambda)
}
