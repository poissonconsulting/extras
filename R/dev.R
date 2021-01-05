impl_dev <- function(x, mu, dev) {
  sign(x - mu) * sqrt(dev)
}

#' Poisson Deviances
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family devdist
#' @export
#'
#' @examples
#' dev_pois(c(1,3.5,4), 3)
dev_pois <- function(x, lambda, res = FALSE) {
  dev <- x * log(x/lambda) - (x - lambda)
  dev[x == 0] <- lambda
  dev <- pmax(dev, 0)
  dev <- dev * 2
  if(vld_false(res)) return(dev)
  impl_dev(x, lambda, dev)
}

#' Normal Deviances
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family devdist
#' @export
#'
#' @examples
#' dev_norm(c(-2:2))
dev_norm <- function(x, mean = 0, sd = 1, res = FALSE) {
  dev <- (x - mean)^2/sd^2
  dev <- dev * 2
  if(vld_false(res)) return(dev)
  impl_dev(x, mean, dev)
}

#' Log-Normal Deviances
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family devdist
#' @export
#'
#' @examples
#' dev_lnorm(exp(-2:2))
dev_lnorm <- function(x, meanlog = 0, sdlog = 1, res = FALSE) {
  x <- pmax(x, 0)
  dev_norm(log(x), mean = meanlog, sd = sdlog, res = res)
}

#' Binomial Deviances
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family devdist
#' @export
#'
#' @examples
#' dev_binom(c(0, 1, 2), 2, 0.3)
dev_binom <- function(x, size, prob, res = FALSE) {
  mu <- size * prob
  dev1 <- x * log(x/mu)
  dev2 <- (size - x) * log((size-x)/(size-mu))
  dev1[x == 0] <- 0
  dev1 <- pmax(dev1, 0)
  dev2[x == size] <- 0
  dev2 <- pmax(dev2, 0)
  dev <- dev1 + dev2
  dev <- dev * 2
  if(vld_false(res)) return(dev)
  impl_dev(x, mu, dev)
}

#' Bernoulli Deviances
#'
#' @inheritParams params
#' @param x A vector of 0s and 1s.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family devdist
#' @export
#'
#' @examples
#' dev_bern(c(TRUE, FALSE), 0.7)
dev_bern <- function(x, prob, res = FALSE) {
  dev <- ifelse(x == 1, -log(prob), -log(1-prob))
  dev <- dev * 2
  if(vld_false(res)) return(dev)
  impl_dev(x, prob, dev)
}

#' Gamma-Poisson Deviances
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family devdist
#' @export
#'
#' @examples
#' dev_gamma_pois(c(1,3.5,4), 3, 2)
dev_gamma_pois <- function(x, lambda, theta, res = FALSE) {
  dev1 <- 1/theta * log((1 + lambda * theta) / (1 + x * theta))
  dev2 <- x * log((lambda + x * lambda * theta) / (x + x * lambda * theta))
  dev <- dev1 - dev2
  dev <- pmax(dev, 0)
  dev <- dev * 2
  dev[theta == 0] <- dev_pois(x[theta == 0], lambda[theta == 0], res = FALSE)
  if(vld_false(res)) return(dev)
  impl_dev(x, lambda, dev)
}
