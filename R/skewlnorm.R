#' Skew-Lognormal Distribution
#'
#' The skew-lognormal distribution of a random variable whose natural logarithm
#' follows a [Skew-Normal][dskewnorm] distribution with location `location_log`,
#' scale `scale_log` and `shape_log`.
#' It reduces to the Log-Normal distribution when `shape_log = 0`.
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#' @param location_log A numeric vector of location parameters of `log(x)`.
#' @param scale_log A non-negative numeric vector of scale parameters of `log(x)`.
#' @param shape_log A numeric vector of shape parameters of `log(x)`.
#' Negative values result in leftward skew, while positive values result in rightward skew.
#' @param ... Unused.
#' @param meanlog `r lifecycle::badge("deprecated")` A numeric vector of location parameters of `log(x)`.
#' Described as "a numeric vector of the means on the log scale" prior to v.
#' 0.10.1.
#' Will be removed in a future version.
#' @param sdlog `r lifecycle::badge("deprecated")` A non-negative numeric vector of scale parameters of
#' `log(x)`.
#' Described as "a non-negative numeric vector of the standard deviations on the
#' log scale" prior to v. 0.10.1.
#' Will be removed in a future version.
#' @param shape `r lifecycle::badge("deprecated")` A numeric vector of shape parameters of `log(x)`.
#' Will be removed in a future version.
#' @return `dskewlnorm` gives the density, `pskewlnorm` gives the distribution function, `qskewlnorm` gives the quantile function, and `rskewlnorm` generates random deviates.
#' `pskewlnorm` and `qskewlnorm` use the lower tail probability.
#' @family skewlnorm
#' @rdname skewlnorm
#' @export
#'
#' @examplesIf rlang::is_installed("sn")
#' dskewlnorm(x = 1:5, location_log = 0, scale_log = 1, shape_log = 0.1)
#' dskewlnorm(x = 1:5, location_log = 0, scale_log = 1, shape_log = -1)
#' qskewlnorm(p = c(0.1, 0.4), location_log = 0, scale_log = 1, shape_log = 0.1)
#' qskewlnorm(p = c(0.1, 0.4), location_log = 0, scale_log = 1, shape_log = -1)
#' pskewlnorm(q = 1:5, location_log = 0, scale_log = 1, shape_log = 0.1)
#' pskewlnorm(q = 1:5, location_log = 0, scale_log = 1, shape_log = -1)
#' rskewlnorm(n = 3, location_log = 0, scale_log = 1, shape_log = 0.1)
#' rskewlnorm(n = 3, location_log = 0, scale_log = 1, shape_log = -1)
dskewlnorm <- function(x, location_log = 0, scale_log = 1, shape_log = 0,
                       log = FALSE, ..., meanlog, sdlog, shape) {
  rlang::check_installed("sn")
  if (!missing(meanlog)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "dskewlnorm(meanlog)",
                              id = "dskewlnorm location_log",
                              with = "dskewlnorm(location_log)")
    location_log <- meanlog
  }
  if (!missing(sdlog)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "dskewlnorm(sdlog)",
                              id = "dskewlnorm scale_log",
                              with = "dskewlnorm(scale_log)")
    scale_log <- sdlog
  }
  if (!missing(shape)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "dskewlnorm(shape)",
                              id = "dskewlnorm shape_log",
                              with = "dskewlnorm(shape_log)")
    shape_log <- shape
  }
  chk_unused(...)
  chk_gte(scale_log)
  nulls <- any(is.null(x), is.null(location_log), is.null(scale_log), is.null(shape_log))
  if (nulls) {
    stop("invalid arguments")
  }
  lengths <- as.logical(length(x)) +
    as.logical(length(location_log)) +
    as.logical(length(scale_log)) +
    as.logical(length(shape_log))
  if (lengths >= 4) {
    nas <- any(is.na(x), is.na(location_log), is.na(scale_log), is.na(shape_log))
    if (!nas) chk_compatible_lengths(x, location_log, scale_log, shape_log)
  }
  character <- any(
    is.character(x),
    is.character(location_log),
    is.character(scale_log),
    is.character(shape_log)
  )
  if (lengths < 4 && !character) {
    return(vector(mode = "numeric"))
  }
  chk_false(character)
  na_shape_log <- is.na(shape_log)
  shape_log[na_shape_log] <- 0
  logx <- suppressWarnings(log(x))
  log_lik <- sn::dsn(
    x = logx,
    xi = location_log,
    omega = scale_log,
    alpha = shape_log,
    log = TRUE
  ) -
    logx
  xr <- rep_len(x, length(log_lik))
  log_lik[!is.na(xr) & xr <= 0] <- -Inf
  lik <- if (isTRUE(log)) log_lik else exp(log_lik)
  lik[na_shape_log] <- NA_real_
  lik
}

#' @rdname skewlnorm
#' @export
pskewlnorm <- function(q, location_log = 0, scale_log = 1, shape_log = 0, ...,
                       meanlog, sdlog, shape) {
  rlang::check_installed("sn")
  if (!missing(meanlog)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "pskewlnorm(meanlog)",
                              id = "pskewlnorm location_log",
                              with = "pskewlnorm(location_log)")
    location_log <- meanlog
  }
  if (!missing(sdlog)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "pskewlnorm(sdlog)",
                              id = "pskewlnorm scale_log",
                              with = "pskewlnorm(scale_log)")
    scale_log <- sdlog
  }
  if (!missing(shape)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "pskewlnorm(shape)",
                              id = "pskewlnorm shape_log",
                              with = "pskewlnorm(shape_log)")
    shape_log <- shape
  }
  chk_unused(...)
  chk_gte(scale_log)
  nulls <- any(is.null(q), is.null(location_log), is.null(scale_log), is.null(shape_log))
  if (nulls) {
    stop("invalid arguments")
  }
  lengths <- as.logical(length(q)) +
    as.logical(length(location_log)) +
    as.logical(length(scale_log)) +
    as.logical(length(shape_log))
  if (lengths >= 4) {
    nas <- any(is.na(q), is.na(location_log), is.na(scale_log), is.na(shape_log))
    if (!nas) chk_compatible_lengths(q, location_log, scale_log, shape_log)
  }
  character <- any(
    is.character(q),
    is.character(location_log),
    is.character(scale_log),
    is.character(shape_log)
  )
  if (lengths < 4 && !character) {
    return(vector(mode = "numeric"))
  }
  chk_false(character)
  na_shape_log <- is.na(shape_log)
  shape_log[na_shape_log] <- 0
  logq <- suppressWarnings(log(q))
  logq[!is.na(q) & q <= 0] <- -Inf
  p <- mapply(sn::psn, x = logq, xi = location_log, omega = scale_log, alpha = shape_log)
  p[na_shape_log] <- NA_real_
  p
}

#' @rdname skewlnorm
#' @export
qskewlnorm <- function(p, location_log = 0, scale_log = 1, shape_log = 0, ...,
                       meanlog, sdlog, shape) {
  rlang::check_installed("sn")
  if (!missing(meanlog)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "qskewlnorm(meanlog)",
                              id = "qskewlnorm location_log",
                              with = "qskewlnorm(location_log)")
    location_log <- meanlog
  }
  if (!missing(sdlog)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "qskewlnorm(sdlog)",
                              id = "qskewlnorm scale_log",
                              with = "qskewlnorm(scale_log)")
    scale_log <- sdlog
  }
  if (!missing(shape)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "qskewlnorm(shape)",
                              id = "qskewlnorm shape_log",
                              with = "qskewlnorm(shape_log)")
    shape_log <- shape
  }
  chk_unused(...)
  chk_gte(scale_log)
  chk_gte(p)
  chk_lte(p, 1)
  nulls <- any(is.null(p), is.null(location_log), is.null(scale_log), is.null(shape_log))
  if (nulls) {
    stop("invalid arguments")
  }
  lengths <- as.logical(length(p)) +
    as.logical(length(location_log)) +
    as.logical(length(scale_log)) +
    as.logical(length(shape_log))
  if (lengths >= 4) {
    nas <- any(is.na(p), is.na(location_log), is.na(scale_log), is.na(shape_log))
    if (!nas) chk_compatible_lengths(p, location_log, scale_log, shape_log)
  }
  character <- any(
    is.character(p),
    is.character(location_log),
    is.character(scale_log),
    is.character(shape_log)
  )
  if (lengths < 4 && !character) {
    return(vector(mode = "numeric"))
  }
  chk_false(character)
  na_shape_log <- is.na(shape_log)
  shape_log[na_shape_log] <- 0
  na_sd <- is.na(scale_log)
  scale_log[na_sd] <- 0.1
  q <- mapply(sn::qsn, p = p, xi = location_log, omega = scale_log, alpha = shape_log)
  q <- exp(q)
  q[na_shape_log] <- NA_real_
  q[na_sd] <- NA_real_
  q
}

#' @rdname skewlnorm
#' @export
rskewlnorm <- function(n = 1, location_log = 0, scale_log = 1, shape_log = 0,
                       ..., meanlog, sdlog, shape) {
  rlang::check_installed("sn")
  if (!missing(meanlog)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "rskewlnorm(meanlog)",
                              id = "rskewlnorm location_log",
                              with = "rskewlnorm(location_log)")
    location_log <- meanlog
  }
  if (!missing(sdlog)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "rskewlnorm(sdlog)",
                              id = "rskewlnorm scale_log",
                              with = "rskewlnorm(scale_log)")
    scale_log <- sdlog
  }
  if (!missing(shape)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "rskewlnorm(shape)",
                              id = "rskewlnorm shape_log",
                              with = "rskewlnorm(shape_log)")
    shape_log <- shape
  }
  chk_unused(...)
  chk_gte(n)
  chk_lt(n, Inf)
  chk_not_any_na(n)
  chk_gte(scale_log)
  nulls <- any(is.null(n), is.null(location_log), is.null(scale_log), is.null(shape_log))
  if (nulls) {
    stop("invalid arguments")
  }
  lengths <- as.logical(length(n)) +
    as.logical(length(location_log)) +
    as.logical(length(scale_log)) +
    as.logical(length(shape_log))
  character <- any(
    is.character(n),
    is.character(location_log),
    is.character(scale_log),
    is.character(shape_log)
  )
  if (lengths < 4 && !character) {
    return(vector(mode = "numeric"))
  }
  chk_whole_number(n)
  if (lengths >= 4 && n != 0L) {
    nas <- any(is.na(n), is.na(location_log), is.na(scale_log), is.na(shape_log))
    if (!nas) {
      chk_compatible_lengths(rep(1, n), location_log, scale_log, shape_log)
    }
  }
  chk_false(character)
  ran <- exp(sn::rsn(n, xi = location_log, omega = scale_log, alpha = shape_log))
  attributes(ran) <- NULL
  if (n == 0L) {
    return(ran)
  }
  ran[1:n]
}
