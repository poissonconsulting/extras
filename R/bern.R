#' Bernoulli Distribution
#'
#' @inheritParams params
#' @param x A vector of 0s and 1s.
#'
#' @return An numeric vector of the random samples.
#' @family bern
#' @rdname bern
#' @export
#'
#' @examples
#' dbern(1, 0.5)
dbern <- function(x, prob, log = FALSE)  {
  stats::dbinom(x, size = 1, prob = prob, log = log)
}

#' @rdname bern
#' @export
pbern <- function(q, prob, lower.tail = TRUE, log = FALSE)  {
  stats::pbinom(q, size = 1, prob = prob, lower.tail = lower.tail,
         log.p = log)
}

#' @rdname bern
#' @export
qbern <- function(p, prob, lower.tail = TRUE, log = FALSE)  {
  stats::qbinom(p, size = 1, prob = prob, lower.tail = lower.tail,
         log.p = log)
}

#' @rdname bern
#' @export
rbern <- function(n, prob)  {
  stats::rbinom(n, size = 1, prob = prob)
}
