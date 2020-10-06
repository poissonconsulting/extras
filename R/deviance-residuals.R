impl_dev <- function(x, mu, dev) {
  sign(x - mu) * sqrt(2 * dev)
}

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
#' devpois(c(1,3.5,4), 3)
devpois <- function(x, lambda) {
  dev <- x * log(x/lambda) - (x - lambda)
  dev[x == 0] <- 0
  dev <- pmax(dev, 0)
  impl_dev(x, lambda, dev)
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
  dev <- (x - mean)^2/sd^2
  impl_dev(x, mean, dev)
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
#' devbinom(c(0, 1, 2), 2, 0.3)
devbinom <- function(x, size, prob) {
  mu <- size * prob
  dev1 <- x * log(x/mu)
  dev2 <- (size - x) * log((size-x)/(size-mu))
  dev1[x == 0] <- 0
  dev1 <- pmax(dev1, 0)
  dev2[x == size] <- 0
  dev2 <- pmax(dev2, 0)
  dev <- dev1 + dev2
  impl_dev(x, mu, dev)
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
  impl_dev(x, prob, dev)
}
