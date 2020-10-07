impl_dev <- function(x, mu, dev) {
  sign(x - mu) * sqrt(dev)
}

#' Poisson Deviance
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#' @param lambda A non-negative numeric vector of means.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family devdist
#' @export
#'
#' @examples
#' devpois(c(1,3.5,4), 3, residual = TRUE)
devpois <- function(x, lambda, residual = FALSE) {
  dev <- x * log(x/lambda) - (x - lambda)
  dev[x == 0] <- 0
  dev <- pmax(dev, 0)
  dev <- dev * 2
  if(vld_false(residual)) return(dev)
  impl_dev(x, lambda, dev)
}

#' Normal Deviance
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#' @param mean A numeric vector of the means.
#' @param sd A non-negative numeric vector of the standard deviations.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family devdist
#' @export
#'
#' @examples
#' devnorm(c(-2:2), residual = TRUE)
devnorm <- function(x, mean = 0, sd = 1, residual = FALSE) {
  dev <- (x - mean)^2/sd^2
  dev <- dev * 2
  if(vld_false(residual)) return(dev)
  impl_dev(x, mean, dev)
}

#' Log-Normal Deviance
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#' @param meanlog A numeric vector of the means on the log scale.
#' @param sdlog A non-negative numeric vector of the standard deviations on the log scale.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family devdist
#' @export
#'
#' @examples
#' devlnorm(exp(-2:2), residual = TRUE)
devlnorm <- function(x, meanlog = 0, sdlog = 1, residual = FALSE) {
  x <- pmax(x, 0)
  devnorm(log(x), mean = meanlog, sd = sdlog, residual = residual)
}

#' Binomial Deviance
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#' @param size A non-negative whole numeric vector of the number of trials.
#' @param prob A numeric vector of values between 0 and 1 o the probability of success of each trial.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family devdist
#' @export
#'
#' @examples
#' devbinom(c(0, 1, 2), 2, 0.3, residual = TRUE)
devbinom <- function(x, size, prob, residual = FALSE) {
  mu <- size * prob
  dev1 <- x * log(x/mu)
  dev2 <- (size - x) * log((size-x)/(size-mu))
  dev1[x == 0] <- 0
  dev1 <- pmax(dev1, 0)
  dev2[x == size] <- 0
  dev2 <- pmax(dev2, 0)
  dev <- dev1 + dev2
  dev <- dev * 2
  if(vld_false(residual)) return(dev)
  impl_dev(x, mu, dev)
}

#' Bernoulli Deviance
#'
#' @inheritParams params
#' @param x A logical vector.
#' @param prob A numeric vector of values between 0 and 1 of the probability of TRUE.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family devdist
#' @export
#'
#' @examples
#' devbern(c(TRUE, FALSE), 0.7, residual = TRUE)
devbern <- function(x, prob, residual = FALSE) {
  dev <- ifelse(x == 1, -log(prob), -log(1-prob))
  dev <- dev * 2
  if(vld_false(residual)) return(dev)
  impl_dev(x, prob, dev)
}
