#' Variance
#'
#' @param x A numeric object of MCMC values.
#' @inheritParams params
#' @return A number.
#' @family summary
#' @export
#' @examples
#' variance(1:10)
variance <- function(x, na_rm = FALSE) {
  chk_numeric(x)
  if(anyNA(x)) {
    if(isFALSE(na_rm)) return(NA_real_)
    x <- as.vector(x)
    x <- x[!is.na(x)]
  }
  if(length(x) < 1) return(NA_real_)
  stats::var(x)
}

#' Skewness
#'
#' @param x A numeric object of MCMC values.
#' @inheritParams params
#' @return A number.
#' @family summary
#' @export
#' @examples
#' skewness(1:10)
skewness <- function(x, na_rm = FALSE) {
  chk_numeric(x)
  if(anyNA(x)) {
    if(isFALSE(na_rm)) return(NA_real_)
    x <- as.vector(x)
    x <- x[!is.na(x)]
  }
  if(length(x) < 2) return(NA_real_)
  n <- length(x)
  mu <- xtr_mean(x)
  (sum((x - mu)^3)/n) / (sum((x - mu)^2)/n)^(3/2)
}

#' Kurtosis
#'
#' @param x A numeric object of MCMC values.
#' @inheritParams params
#' @return A number.
#' @family summary
#' @export
#' @examples
#' kurtosis(1:10)
kurtosis <- function(x, na_rm = FALSE) {
  chk_numeric(x)
  if(anyNA(x)) {
    if(isFALSE(na_rm)) return(NA_real_)
    x <- as.vector(x)
    x <- x[!is.na(x)]
  }
  if(length(x) < 2) return(NA_real_)
  n <- length(x)
  mu <- xtr_mean(x)
  n * sum((x - mu)^4)/(sum((x - mu)^2)^2)
}
