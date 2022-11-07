#' Bernoulli Deviances
#'
#' @inheritParams params
#' @param x A vector of 0s and 1s.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family dev_dist
#' @export
#'
#' @examples
#' dev_bern(c(TRUE, FALSE), 0.7)
dev_bern <- function(x, prob = 0.5, res = FALSE) {
  dev <- ifelse(x == 1, -log(prob), -log(1-prob))
  dev <- dev * 2
  if(vld_false(res)) return(dev)
  dev_res(x, prob, dev)
}

#' Binomial Deviances
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family dev_dist
#' @export
#'
#' @examples
#' dev_binom(c(0, 1, 2), 2, 0.3)
dev_binom <- function(x, size = 1, prob = 0.5, res = FALSE) {
  mu <- size * prob
  dev1 <- x * log(x/mu)
  dev2 <- (size - x) * log((size-x)/(size-mu))
  dev1[!is.na(x) & x == 0] <- 0
  dev2[!is.na(x) & x == size] <- 0
  dev <- dev1 + dev2
  dev <- dev * 2
  if(vld_false(res)) return(dev)
  dev_res(x, mu, dev)
}

#' Gamma Deviances
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family dev_dist
#' @export
#'
#' @examples
#' dev_gamma(c(0, 1, 2), 1, 2)
dev_gamma <- function(x, shape = 1, rate = 1, res = FALSE) {
  mu <- shape / rate
  dev <- -log(x / mu) + ((x - mu) / mu)
  dev <- 2 * dev
  if(vld_false(res)) return(dev)
  dev_res(x, mu, dev)
}

#' Gamma-Poisson Deviances
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family dev_dist
#' @export
#'
#' @examples
#' dev_gamma_pois(c(1,3.5,4), 3, 2)
dev_gamma_pois <- function(x, lambda = 1, theta = 0, res = FALSE) {
  dev1 <- 1/theta * log((1 + lambda * theta) / (1 + x * theta))
  dev2 <- x * log((lambda + x * lambda * theta) / (x + x * lambda * theta))
  dev2[!is.na(x) & x == 0] <- 0
  dev <- dev1 - dev2
  dev <- dev * 2
  theta0 <- !is.na(theta) & theta == 0
  dev[theta0] <- dev_pois(x[theta0], lambda[theta0], res = FALSE)
  if(vld_false(res)) return(dev)
  dev_res(x, lambda, dev)
}

#' Zero-Inflated Gamma-Poisson Deviances
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family dev_dist # make live when complete
#' @export
#'
#' @examples
#' dev_gamma_pois_zi(c(1,3.5,4), 3, 2)
dev_gamma_pois_zi <- function(x, lambda = 1, theta = 0, prob = 0, res = FALSE) {
  dev <- log_lik_gamma_pois_zi(x, lambda = x, theta = theta, prob = 0) -
    log_lik_gamma_pois_zi(x, lambda = lambda, theta = theta, prob = prob)
  dev <- dev * 2
  if(vld_false(res)) return(dev)
  dev_res(x, lambda * (1 - prob), dev)
}

#' Log-Normal Deviances
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family dev_dist
#' @export
#'
#' @examples
#' dev_lnorm(exp(-2:2))
dev_lnorm <- function(x, meanlog = 0, sdlog = 1, res = FALSE) {
  x <- pmax(x, 0)
  dev_norm(log(x), mean = meanlog, sd = sdlog, res = res)
}

#' Negative Binomial Deviances
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family dev_dist
#' @export
#'
#' @examples
#' dev_neg_binom(c(1, 2, 5), 2, 3)
dev_neg_binom <- function(x, lambda = 1, theta = 0, res = FALSE) {
  dev_gamma_pois(x, lambda = lambda, theta = theta, res = res)
}

#' Normal Deviances
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family dev_dist
#' @export
#'
#' @examples
#' dev_norm(c(-2:2))
dev_norm <- function(x, mean = 0, sd = 1, res = FALSE) {
  dev <- ((x - mean)/sd)^2
  if(vld_false(res)) return(dev)
  dev_res(x, mean, dev)
}

#' Poisson Deviances
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family dev_dist
#' @export
#'
#' @examples
#' dev_pois(c(1,3.5,4), 3)
dev_pois <- function(x, lambda, res = FALSE) {
  dev <- x * log(x/lambda) - (x - lambda)
  zero <- !is.na(x) & x == 0
  if(any(zero)) {
    if(length(lambda) == 1) {
      lambda <- rep(lambda, length(x))
    }
    dev[zero] <- lambda[zero]
  }
  dev <- dev * 2
  if(vld_false(res)) return(dev)
  dev_res(x, lambda, dev)
}

#' Zero-Inflated Poisson Deviances
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family dev_dist
#' @export
#'
#' @examples
#' dev_pois(c(1,3.5,4), 3)
dev_pois_zi <- function(x, lambda, prob = 0, res = FALSE) {
  dev1 <- -x + x * log(x) - log(factorial(x))
  dev1[x == 0] <- 0
  dev0 <- log(prob + (1 - prob) * exp(-lambda))
  devg0 <- log(1-prob) - lambda + x * log(lambda) - log(factorial(x))
  dev <- dev1 - dev0
  devg0 <- dev1 - devg0
  g0 <- !is.na(x) & x > 0
  dev[g0] <- devg0[g0]
  dev <- dev * 2
  if(vld_false(res)) return(dev)
  dev_res(x, lambda * (1 - prob), dev)
}

#' Student's t Deviances
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#'
#' @return An numeric vector of the corresponding deviances or deviance residuals.
#' @family dev_dist
#' @export
#'
#' @examples
#' dev_student(c(1,3.5,4), 3)
dev_student <- function(x, mean = 0, sd = 1, theta = 0, res = FALSE) {
  dev <- log_lik_student(x, mean = x, sd = sd, theta = theta) -
    log_lik_student(x, mean = mean, sd = sd, theta = theta)
  dev <- dev * 2
  if(vld_false(res)) return(dev)
  dev_res(x, mean, dev)
}

