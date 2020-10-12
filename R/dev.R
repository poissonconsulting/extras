impl_dev <- function(x, mu, dev) {
  sign(x - mu) * sqrt(dev)
}

#' Poisson Deviance
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family devdist
#' @export
#'
#' @examples
#' dev_pois(c(1,3.5,4), 3, residual = TRUE)
dev_pois <- function(x, lambda, residual = FALSE) {
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
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family devdist
#' @export
#'
#' @examples
#' dev_norm(c(-2:2), residual = TRUE)
dev_norm <- function(x, mean = 0, sd = 1, residual = FALSE) {
  dev <- (x - mean)^2/sd^2
  dev <- dev * 2
  if(vld_false(residual)) return(dev)
  impl_dev(x, mean, dev)
}

#' Log-Normal Deviance
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family devdist
#' @export
#'
#' @examples
#' dev_lnorm(exp(-2:2), residual = TRUE)
dev_lnorm <- function(x, meanlog = 0, sdlog = 1, residual = FALSE) {
  x <- pmax(x, 0)
  dev_norm(log(x), mean = meanlog, sd = sdlog, residual = residual)
}

#' Binomial Deviance
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family devdist
#' @export
#'
#' @examples
#' dev_binom(c(0, 1, 2), 2, 0.3, residual = TRUE)
dev_binom <- function(x, size, prob, residual = FALSE) {
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
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family devdist
#' @export
#'
#' @examples
#' dev_bern(c(TRUE, FALSE), 0.7, residual = TRUE)
dev_bern <- function(x, prob, residual = FALSE) {
  dev <- ifelse(x == 1, -log(prob), -log(1-prob))
  dev <- dev * 2
  if(vld_false(residual)) return(dev)
  impl_dev(x, prob, dev)
}
