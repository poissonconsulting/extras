#' Skew-Lognormal Distribution
#'
#' The skew-lognormal distribution of a random variable whose natural logarithm
#' follows a [Skew-Normal][dskewnorm] distribution with location `locationlog`,
#' scale `scalelog` and `shapelog`.
#' It reduces to the Log-Normal distribution when `shapelog = 0`.
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#' @param locationlog A numeric vector of location parameters of `log(x)`.
#' @param scalelog A non-negative numeric vector of scale parameters of `log(x)`.
#' @param shapelog A numeric vector of shape parameters of `log(x)`.
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
#' dskewlnorm(x = 1:5, locationlog = 0, scalelog = 1, shapelog = 0.1)
#' dskewlnorm(x = 1:5, locationlog = 0, scalelog = 1, shapelog = -1)
#' qskewlnorm(p = c(0.1, 0.4), locationlog = 0, scalelog = 1, shapelog = 0.1)
#' qskewlnorm(p = c(0.1, 0.4), locationlog = 0, scalelog = 1, shapelog = -1)
#' pskewlnorm(q = 1:5, locationlog = 0, scalelog = 1, shapelog = 0.1)
#' pskewlnorm(q = 1:5, locationlog = 0, scalelog = 1, shapelog = -1)
#' rskewlnorm(n = 3, locationlog = 0, scalelog = 1, shapelog = 0.1)
#' rskewlnorm(n = 3, locationlog = 0, scalelog = 1, shapelog = -1)
dskewlnorm <- function(x, locationlog = 0, scalelog = 1, shapelog = 0,
                       log = FALSE, ..., meanlog, sdlog, shape) {
  rlang::check_installed("sn")
  if (!missing(meanlog)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "dskewlnorm(meanlog)",
                              id = "dskewlnorm locationlog",
                              with = "dskewlnorm(locationlog)")
    locationlog <- meanlog
  }
  if (!missing(sdlog)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "dskewlnorm(sdlog)",
                              id = "dskewlnorm scalelog",
                              with = "dskewlnorm(scalelog)")
    scalelog <- sdlog
  }
  if (!missing(shape)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "dskewlnorm(shape)",
                              id = "dskewlnorm shapelog",
                              with = "dskewlnorm(shapelog)")
    shapelog <- shape
  }
  chk_unused(...)
  chk_gte(scalelog)
  nulls <- any(is.null(x), is.null(locationlog), is.null(scalelog), is.null(shapelog))
  if (nulls) {
    stop("invalid arguments")
  }
  lengths <- as.logical(length(x)) +
    as.logical(length(locationlog)) +
    as.logical(length(scalelog)) +
    as.logical(length(shapelog))
  if (lengths == 4) {
    nas <- any(is.na(x), is.na(locationlog), is.na(scalelog), is.na(shapelog))
    if (!nas) chk_compatible_lengths(x, locationlog, scalelog, shapelog)
  }
  character <- any(
    is.character(x),
    is.character(locationlog),
    is.character(scalelog),
    is.character(shapelog)
  )
  if (lengths < 4 && !character) {
    return(vector(mode = "numeric"))
  }
  chk_false(character)
  na_shapelog <- is.na(shapelog)
  shapelog[na_shapelog] <- 0
  logx <- suppressWarnings(log(x))
  log_lik <- sn::dsn(
    x = logx,
    xi = locationlog,
    omega = scalelog,
    alpha = shapelog,
    log = TRUE
  ) -
    logx
  xr <- rep_len(x, length(log_lik))
  log_lik[!is.na(xr) & xr <= 0] <- -Inf
  lik <- if (isTRUE(log)) log_lik else exp(log_lik)
  lik[na_shapelog] <- NA_real_
  lik
}

#' @rdname skewlnorm
#' @export
pskewlnorm <- function(q, locationlog = 0, scalelog = 1, shapelog = 0, ...,
                       meanlog, sdlog, shape) {
  rlang::check_installed("sn")
  if (!missing(meanlog)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "pskewlnorm(meanlog)",
                              id = "pskewlnorm locationlog",
                              with = "pskewlnorm(locationlog)")
    locationlog <- meanlog
  }
  if (!missing(sdlog)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "pskewlnorm(sdlog)",
                              id = "pskewlnorm scalelog",
                              with = "pskewlnorm(scalelog)")
    scalelog <- sdlog
  }
  if (!missing(shape)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "pskewlnorm(shape)",
                              id = "pskewlnorm shapelog",
                              with = "pskewlnorm(shapelog)")
    shapelog <- shape
  }
  chk_unused(...)
  chk_gte(scalelog)
  nulls <- any(is.null(q), is.null(locationlog), is.null(scalelog), is.null(shapelog))
  if (nulls) {
    stop("invalid arguments")
  }
  lengths <- as.logical(length(q)) +
    as.logical(length(locationlog)) +
    as.logical(length(scalelog)) +
    as.logical(length(shapelog))
  if (lengths == 4) {
    nas <- any(is.na(q), is.na(locationlog), is.na(scalelog), is.na(shapelog))
    if (!nas) chk_compatible_lengths(q, locationlog, scalelog, shapelog)
  }
  character <- any(
    is.character(q),
    is.character(locationlog),
    is.character(scalelog),
    is.character(shapelog)
  )
  if (lengths < 4 && !character) {
    return(vector(mode = "numeric"))
  }
  chk_false(character)
  na_shapelog <- is.na(shapelog)
  shapelog[na_shapelog] <- 0
  logq <- suppressWarnings(log(q))
  logq[!is.na(q) & q <= 0] <- -Inf
  p <- mapply(sn::psn, x = logq, xi = locationlog, omega = scalelog, alpha = shapelog)
  p[na_shapelog] <- NA_real_
  p
}

#' @rdname skewlnorm
#' @export
qskewlnorm <- function(p, locationlog = 0, scalelog = 1, shapelog = 0, ...,
                       meanlog, sdlog, shape) {
  rlang::check_installed("sn")
  if (!missing(meanlog)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "qskewlnorm(meanlog)",
                              id = "qskewlnorm locationlog",
                              with = "qskewlnorm(locationlog)")
    locationlog <- meanlog
  }
  if (!missing(sdlog)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "qskewlnorm(sdlog)",
                              id = "qskewlnorm scalelog",
                              with = "qskewlnorm(scalelog)")
    scalelog <- sdlog
  }
  if (!missing(shape)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "qskewlnorm(shape)",
                              id = "qskewlnorm shapelog",
                              with = "qskewlnorm(shapelog)")
    shapelog <- shape
  }
  chk_unused(...)
  chk_gte(scalelog)
  chk_gte(p)
  chk_lte(p, 1)
  nulls <- any(is.null(p), is.null(locationlog), is.null(scalelog), is.null(shapelog))
  if (nulls) {
    stop("invalid arguments")
  }
  lengths <- as.logical(length(p)) +
    as.logical(length(locationlog)) +
    as.logical(length(scalelog)) +
    as.logical(length(shapelog))
  if (lengths == 4) {
    nas <- any(is.na(p), is.na(locationlog), is.na(scalelog), is.na(shapelog))
    if (!nas) chk_compatible_lengths(p, locationlog, scalelog, shapelog)
  }
  character <- any(
    is.character(p),
    is.character(locationlog),
    is.character(scalelog),
    is.character(shapelog)
  )
  if (lengths < 4 && !character) {
    return(vector(mode = "numeric"))
  }
  chk_false(character)
  na_shapelog <- is.na(shapelog)
  shapelog[na_shapelog] <- 0
  na_sd <- is.na(scalelog)
  scalelog[na_sd] <- 0.1
  q <- mapply(sn::qsn, p = p, xi = locationlog, omega = scalelog, alpha = shapelog)
  q <- exp(q)
  q[na_shapelog] <- NA_real_
  q[na_sd] <- NA_real_
  q
}

#' @rdname skewlnorm
#' @export
rskewlnorm <- function(n = 1, locationlog = 0, scalelog = 1, shapelog = 0,
                       ..., meanlog, sdlog, shape) {
  rlang::check_installed("sn")
  if (!missing(meanlog)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "rskewlnorm(meanlog)",
                              id = "rskewlnorm locationlog",
                              with = "rskewlnorm(locationlog)")
    locationlog <- meanlog
  }
  if (!missing(sdlog)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "rskewlnorm(sdlog)",
                              id = "rskewlnorm scalelog",
                              with = "rskewlnorm(scalelog)")
    scalelog <- sdlog
  }
  if (!missing(shape)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "rskewlnorm(shape)",
                              id = "rskewlnorm shapelog",
                              with = "rskewlnorm(shapelog)")
    shapelog <- shape
  }
  chk_unused(...)
  chk_gte(n)
  chk_lt(n, Inf)
  chk_not_any_na(n)
  chk_gte(scalelog)
  nulls <- any(is.null(n), is.null(locationlog), is.null(scalelog), is.null(shapelog))
  if (nulls) {
    stop("invalid arguments")
  }
  lengths <- as.logical(length(n)) +
    as.logical(length(locationlog)) +
    as.logical(length(scalelog)) +
    as.logical(length(shapelog))
  character <- any(
    is.character(n),
    is.character(locationlog),
    is.character(scalelog),
    is.character(shapelog)
  )
  if (lengths < 4 && !character) {
    return(vector(mode = "numeric"))
  }
  chk_whole_number(n)
  if (lengths == 4 && n != 0L) {
    nas <- any(is.na(n), is.na(locationlog), is.na(scalelog), is.na(shapelog))
    if (!nas) {
      chk_compatible_lengths(rep(1, n), locationlog, scalelog, shapelog)
    }
  }
  chk_false(character)
  ran <- exp(sn::rsn(n, xi = locationlog, omega = scalelog, alpha = shapelog))
  attributes(ran) <- NULL
  if (n == 0L) {
    return(ran)
  }
  ran[1:n]
}
