# Reference Poisson-lognormal log-likelihood by stats::integrate() over the
# standard normal deviate z, centred on the mode of the integrand.
log_lik_pois_lnorm_integrate <- function(x, lambda, sdlog) {
  mapply(
    function(x, lambda, sdlog) {
      log_integrand <- function(z) {
        stats::dpois(x, lambda * exp(sdlog * z - sdlog^2 / 2), log = TRUE) +
          stats::dnorm(z, log = TRUE)
      }
      score <- function(z) {
        x * sdlog - sdlog * lambda * exp(sdlog * z - sdlog^2 / 2) - z
      }
      upper <- (log(max(x + 0.5, lambda) / lambda) + sdlog^2 / 2) / sdlog
      mode <- stats::uniroot(
        score,
        c(-sdlog * lambda - 1, upper),
        tol = 1e-13
      )$root
      log_max <- log_integrand(mode)
      integrand <- function(z) {
        value <- exp(log_integrand(z) - log_max)
        value[!is.finite(value)] <- 0
        value
      }
      width <- 40 /
        sqrt(sdlog^2 * lambda * exp(sdlog * mode - sdlog^2 / 2) + 1)
      area <- function(lower, upper) {
        stats::integrate(
          integrand,
          lower,
          upper,
          rel.tol = 1e-10,
          abs.tol = 0,
          subdivisions = 10000L
        )$value
      }
      log(area(mode - width, mode) + area(mode, mode + width)) + log_max
    },
    x,
    lambda,
    sdlog
  )
}
