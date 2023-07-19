#' Skew-Normal Distribution
#'
#' @inheritParams params
#' @param x A vector of values.
#'
#' @return `dskewnorm` gives the density, `pskewnorm` gives the distribution function, `qskewnorm` gives the quantile function, and `rskewnorm` generates random deviates.
#' @family skewnorm
#' @rdname skewnorm
#' @export
#'
#' @examples
#' dskewnorm(x = -3:3, mean = 0, sd = 1, shape = 0.1)
#' dskewnorm(x = -3:3, mean = 0, sd = 1, shape = -1)
#' rskewnorm(n = 3, mean = 0, sd = 1, shape = 0.1)
#' rskewnorm(n = 3, mean = 0, sd = 1, shape = -1)
#' qskewnorm(p = c(0.1, 0.4), mean = 0, sd = 1, shape = 0.1)
#' qskewnorm(p = c(0.1, 0.4), mean = 0, sd = 1, shape = -1)
#' pskewnorm(q = -3:3, mean = 0, sd = 1, shape = 0.1)
#' pskewnorm(q = -3:3, mean = 0, sd = 1, shape = -1)
dskewnorm <- function(x, mean = 0, sd = 1, shape = 0, log = FALSE)  {
  log_lik <- log(2) - log(sd) + stats::dnorm((x - mean) / sd, 0, 1, log = TRUE) +
    stats::pnorm(shape * (x - mean) / sd, 0, 1, log.p = TRUE)
  lnorm <- log_lik_norm(x = x, mean = mean, sd = sd)
  if (length(shape) == 1) {
    shape <- rep(shape, length(lnorm))
  }
  use_norm <- !is.na(shape) & shape == 0
  log_lik[use_norm] <- lnorm[use_norm]
  out <- if(log) log_lik
    else exp(log_lik)
  out
}

#' @rdname skewnorm
#' @export
pskewnorm <- function(q, mean = 0, sd = 1, shape = 0, lower.tail = TRUE, log = FALSE)  {
  chk_gte(sd)
  chk_length(shape)
  h <- (q - mean) / sd
  int_t <- function(x, h) {
    (exp(-0.5 * h^2 * (1 + x^2))) / (1 + x^2)
  }
  owen_t <- function(h, shape) {
    ot <- 1 / (2 * pi) * integrate(int_t, h = h, lower = 0, upper = shape, abs.tol = 1e-20)$value
    ot
  }
  vot <- sapply(h, owen_t, shape = shape)
  # ot2 <- sn::T.Owen(h, a = shape)
  p <- stats::pnorm(h) - 2 * vot
  # p2 <- sn::psn(q, alpha = shape)
  p[p < 0] <- 0
  if (!lower.tail) p <- (1 - p)
  if (log) p <- log(p)
  ### Unsure about having log.p argument... with shape != 0, get -Inf quite easily.
  use_norm <- !is.na(shape) & shape == 0
  p[use_norm] <- stats::pnorm(q = q, mean = mean, sd = sd, lower.tail = lower.tail, log.p = log)
  p
}

#' @rdname skewnorm
#' @export
qskewnorm <- function(p, mean = 0, sd = 1, shape = 0, lower.tail = TRUE, log = FALSE) {
  chk_gte(sd)
  chk_gte(p)
  chk_lte(p, 1)

  # ???

}



#' @rdname skewnorm
#' @export
rskewnorm <- function(n, mean = 0, sd = 1, shape = 0)  {
  chk_whole_number(n)
  chk_gte(n)
  chk_gte(sd)
  delta <- shape / sqrt(1 + shape^2)
  tn <- matrix(stats::rnorm(2 * n), 2, n, byrow = FALSE)
  chi <- c(abs(tn[1,]))
  nrv <- c(tn[2,])
  z <- delta * chi + sqrt(1 - delta^2) * nrv
  y <- as.vector(mean + sd * z)
  bol <- length(y) > n
  if (bol) y <- y[1:n]
  y
}
