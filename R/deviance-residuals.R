#' Poisson Deviance Residual
#'
#' @param x A non-negative whole numeric vector of values.
#' @param lambda A non-negative numeric vector of means.
#'
#' @return An numeric vector of the corresponding deviance residuals.
#' @family devdist
#' @export
#'
#' @examples
#' # devpois(c(1,3.5,4), 3)
devpois <- function(x, lambda) {
  .NotYetImplemented()
  dev <- x * log(x/lambda) - (x - lambda)
  dev <- pmax(dev, 0)
  sign(x - lambda) * sqrt(2 * dev)
}

#' Normal Deviance Residual
#'
#' @param x A numeric vector of values.
#' @param mean A numeric vector of the means.
#' @param sd A non-negative numeric vector of the standard deviations.
#'
#' @return An numeric vector of the corresponding deviance residuals.
#' @family devdist
#' @export
#'
#' @examples
#' devnorm(c(-2:2))
devnorm <- function(x, mean = 0, sd = 1) {
  dev <- dnorm(mean, mean, sd, log = TRUE) - dnorm(x, mean, sd, log = TRUE)
  sign(x - mean) * sqrt(2 * dev)
}

#' Log-Normal Deviance Residual
#'
#' @param x A numeric vector of values.
#' @param meanlog A numeric vector of the means on the log scale.
#' @param sdlog A non-negative numeric vector of the standard deviations on the log scale.
#'
#' @return An numeric vector of the corresponding deviance residuals.
#' @family devdist
#' @export
#'
#' @examples
#' devlnorm(exp(-2:2))
devlnorm <- function(x, meanlog = 0, sdlog = 1) {
  x <- pmax(x, 0)
  devnorm(log(x), mean = meanlog, sd = sdlog)
}

#' Binomial Deviance Residual
#'
#' @param x A non-negative whole numeric vector of values.
#' @param size A non-negative whole numeric vector of the number of trials.
#' @param prob A numeric vector of values between 0 and 1 o the probability of success of each trial.
#'
#' @return An numeric vector of the corresponding deviance residuals.
#' @family devdist
#' @export
#'
#' @examples
#' # devbinom(c(0, 1, 2), 2, 0.3)
devbinom <- function(x, size, prob) {
  .NotYetImplemented()
  mu <- size * prob
  dev <- x * log(x / mu) + (size - x) * log((size-x)/(size - mu))
  sign(x - mu) * sqrt(2 * dev)
}

#' Bernoulli Deviance Residual
#'
#' @param x A logical vector.
#' @param prob A numeric vector of values between 0 and 1 of the probability of TRUE.
#'
#' @return An numeric vector of the corresponding deviance residuals.
#' @family devdist
#' @export
#'
#' @examples
#' devbern(c(TRUE, FALSE), 0.7)
devbern <- function(x, prob) {
  dev <- ifelse(x == 1, -log(prob), -log(1-prob))
  sign(x - prob) * sqrt(2 * dev)
}
