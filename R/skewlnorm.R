#' Skew-Lognormal Distribution
#'
#' The skew-lognormal distribution of a value `x` whose natural logarithm
#' follows a [Skew-Normal][dskewnorm] distribution with location `meanlog`,
#' scale `sdlog` and `shape`.
#' It reduces to the Log-Normal distribution when `shape = 0`.
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#' @param shape A numeric vector of values.
#'
#' @return `dskewlnorm` gives the density, `pskewlnorm` gives the distribution function, `qskewlnorm` gives the quantile function, and `rskewlnorm` generates random deviates.
#' `pskewlnorm` and `qskewlnorm` use the lower tail probability.
#' @family skewlnorm
#' @rdname skewlnorm
#' @export
#'
#' @examplesIf rlang::is_installed("sn")
#' dskewlnorm(x = 1:5, meanlog = 0, sdlog = 1, shape = 0.1)
#' dskewlnorm(x = 1:5, meanlog = 0, sdlog = 1, shape = -1)
#' qskewlnorm(p = c(0.1, 0.4), meanlog = 0, sdlog = 1, shape = 0.1)
#' qskewlnorm(p = c(0.1, 0.4), meanlog = 0, sdlog = 1, shape = -1)
#' pskewlnorm(q = 1:5, meanlog = 0, sdlog = 1, shape = 0.1)
#' pskewlnorm(q = 1:5, meanlog = 0, sdlog = 1, shape = -1)
#' rskewlnorm(n = 3, meanlog = 0, sdlog = 1, shape = 0.1)
#' rskewlnorm(n = 3, meanlog = 0, sdlog = 1, shape = -1)
dskewlnorm <- function(x, meanlog = 0, sdlog = 1, shape = 0, log = FALSE) {
  rlang::check_installed("sn")
  chk_gte(sdlog)
  nulls <- any(is.null(x), is.null(meanlog), is.null(sdlog), is.null(shape))
  if (nulls) stop("invalid arguments")
  lengths <- as.logical(length(x)) + as.logical(length(meanlog)) + as.logical(length(sdlog)) + as.logical(length(shape))
  if (lengths >= 4) {
    nas <- any(is.na(x), is.na(meanlog), is.na(sdlog), is.na(shape))
    if (!nas) chk_compatible_lengths(x, meanlog, sdlog, shape)
  }
  character <- any(is.character(x), is.character(meanlog), is.character(sdlog), is.character(shape))
  if (lengths < 4 && !character) {
    return(vector(mode = "numeric"))
  }
  chk_false(character)
  na_shape <- is.na(shape)
  shape[na_shape] <- 0
  logx <- suppressWarnings(log(x))
  log_lik <- sn::dsn(x = logx, xi = meanlog, omega = sdlog, alpha = shape, log = TRUE) - logx
  xr <- rep_len(x, length(log_lik))
  log_lik[!is.na(xr) & xr <= 0] <- -Inf
  lik <- if (isTRUE(log)) log_lik else exp(log_lik)
  lik[na_shape] <- NA_real_
  lik
}

#' @rdname skewlnorm
#' @export
pskewlnorm <- function(q, meanlog = 0, sdlog = 1, shape = 0) {
  rlang::check_installed("sn")
  chk_gte(sdlog)
  nulls <- any(is.null(q), is.null(meanlog), is.null(sdlog), is.null(shape))
  if (nulls) stop("invalid arguments")
  lengths <- as.logical(length(q)) + as.logical(length(meanlog)) + as.logical(length(sdlog)) + as.logical(length(shape))
  if (lengths >= 4) {
    nas <- any(is.na(q), is.na(meanlog), is.na(sdlog), is.na(shape))
    if (!nas) chk_compatible_lengths(q, meanlog, sdlog, shape)
  }
  character <- any(is.character(q), is.character(meanlog), is.character(sdlog), is.character(shape))
  if (lengths < 4 && !character) {
    return(vector(mode = "numeric"))
  }
  chk_false(character)
  na_shape <- is.na(shape)
  shape[na_shape] <- 0
  logq <- suppressWarnings(log(q))
  logq[!is.na(q) & q <= 0] <- -Inf
  p <- mapply(sn::psn, x = logq, xi = meanlog, omega = sdlog, alpha = shape)
  p[na_shape] <- NA_real_
  p
}

#' @rdname skewlnorm
#' @export
qskewlnorm <- function(p, meanlog = 0, sdlog = 1, shape = 0) {
  rlang::check_installed("sn")
  chk_gte(sdlog)
  chk_gte(p)
  chk_lte(p, 1)
  nulls <- any(is.null(p), is.null(meanlog), is.null(sdlog), is.null(shape))
  if (nulls) stop("invalid arguments")
  lengths <- as.logical(length(p)) + as.logical(length(meanlog)) + as.logical(length(sdlog)) + as.logical(length(shape))
  if (lengths >= 4) {
    nas <- any(is.na(p), is.na(meanlog), is.na(sdlog), is.na(shape))
    if (!nas) chk_compatible_lengths(p, meanlog, sdlog, shape)
  }
  character <- any(is.character(p), is.character(meanlog), is.character(sdlog), is.character(shape))
  if (lengths < 4 && !character) {
    return(vector(mode = "numeric"))
  }
  chk_false(character)
  na_shape <- is.na(shape)
  shape[na_shape] <- 0
  na_sd <- is.na(sdlog)
  sdlog[na_sd] <- 0.1
  q <- mapply(sn::qsn, p = p, xi = meanlog, omega = sdlog, alpha = shape)
  q <- exp(q)
  q[na_shape] <- NA_real_
  q[na_sd] <- NA_real_
  q
}

#' @rdname skewlnorm
#' @export
rskewlnorm <- function(n = 1, meanlog = 0, sdlog = 1, shape = 0) {
  rlang::check_installed("sn")
  chk_gte(n)
  chk_lt(n, Inf)
  chk_not_any_na(n)
  chk_gte(sdlog)
  nulls <- any(is.null(n), is.null(meanlog), is.null(sdlog), is.null(shape))
  if (nulls) stop("invalid arguments")
  lengths <- as.logical(length(n)) + as.logical(length(meanlog)) + as.logical(length(sdlog)) + as.logical(length(shape))
  character <- any(is.character(n), is.character(meanlog), is.character(sdlog), is.character(shape))
  if (lengths < 4 && !character) {
    return(vector(mode = "numeric"))
  }
  chk_whole_number(n)
  if (lengths >= 4 && n != 0L) {
    nas <- any(is.na(n), is.na(meanlog), is.na(sdlog), is.na(shape))
    if (!nas) {
      chk_compatible_lengths(rep(1, n), meanlog, sdlog, shape)
    }
  }
  chk_false(character)
  ran <- exp(sn::rsn(n, xi = meanlog, omega = sdlog, alpha = shape))
  attributes(ran) <- NULL
  if (n == 0L) {
    return(ran)
  }
  ran[1:n]
}
