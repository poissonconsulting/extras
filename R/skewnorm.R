#' Skew-Normal Distribution
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#' @param shape A numeric vector of shape. Negative values result in leftward skew, while positive values result in rightward skew.
#' @param ... Unused.
#' @param mean `r lifecycle::badge("deprecated")` A numeric vector of the location parameter.
#' Described as "a numeric vector of the means" prior to to v. 0.10.1.
#' Will be removed in a future version.
#' @param sd `r lifecycle::badge("deprecated")` A non-negative numeric vector of the scale parameter.
#' Described as "a non-negative numeric vector of the standard deviations" prior
#' to v. 0.10.1.
#' Will be removed in a future version.
#'
#' @return `dskewnorm` gives the density, `pskewnorm` gives the distribution function, `qskewnorm` gives the quantile function, and `rskewnorm` generates random deviates.
#' `pskewnorm` and `qskewnorm` use the lower tail probability.
#' @family skewnorm
#' @rdname skewnorm
#' @export
#'
#' @examplesIf rlang::is_installed("sn")
#' dskewnorm(x = -2:2, location = 0, scale = 1, shape = 0.1)
#' dskewnorm(x = -2:2, location = 0, scale = 1, shape = -1)
#' qskewnorm(p = c(0.1, 0.4), location = 0, scale = 1, shape = 0.1)
#' qskewnorm(p = c(0.1, 0.4), location = 0, scale = 1, shape = -1)
#' pskewnorm(q = -2:2, location = 0, scale = 1, shape = 0.1)
#' pskewnorm(q = -2:2, location = 0, scale = 1, shape = -1)
#' rskewnorm(n = 3, location = 0, scale = 1, shape = 0.1)
#' rskewnorm(n = 3, location = 0, scale = 1, shape = -1)
dskewnorm <- function(x, location = 0, scale = 1, shape = 0, log = FALSE, ...,
                      mean, sd) {
  rlang::check_installed("sn")

  if (!missing(mean)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "dskewnorm(mean)",
                              id = "dskewnorm location",
                              with = "dskewnorm(location)")
    location <- mean
  }
  if (!missing(sd)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "dskewnorm(sd)",
                              id = "dskewnorm scale",
                              with = "dskewnorm(scale)")
    scale <- sd
  }
  chk_unused(...)
  chk_gte(scale)
  nulls <- any(is.null(x), is.null(location), is.null(scale), is.null(shape))
  if (nulls) {
    stop("invalid arguments")
  }
  lengths <- as.logical(length(x)) +
    as.logical(length(location)) +
    as.logical(length(scale)) +
    as.logical(length(shape))
  if (lengths >= 4) {
    nas <- any(is.na(x), is.na(location), is.na(scale), is.na(shape))
    if (!nas) chk_compatible_lengths(x, location, scale, shape)
  }
  character <- any(
    is.character(x),
    is.character(location),
    is.character(scale),
    is.character(shape)
  )
  if (lengths < 4 && !character) {
    return(vector(mode = "numeric"))
  }
  chk_false(character)
  na_shape <- is.na(shape)
  shape[na_shape] <- 0
  lik <- sn::dsn(x = x, xi = location, omega = scale, alpha = shape, log = log)
  lik[na_shape] <- NA_real_
  lik
}

#' @rdname skewnorm
#' @export
pskewnorm <- function(q, location = 0, scale = 1, shape = 0, ..., mean, sd) {
  rlang::check_installed("sn")
  if (!missing(mean)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "pskewnorm(mean)",
                              id = "pskewnorm location",
                              with = "pskewnorm(location)")
    location <- mean
  }
  if (!missing(sd)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "pskewnorm(sd)",
                              id = "pskewnorm scale",
                              with = "pskewnorm(scale)")
    scale <- sd
  }
  chk_unused(...)
  chk_gte(scale)
  nulls <- any(is.null(q), is.null(location), is.null(scale), is.null(shape))
  if (nulls) {
    stop("invalid arguments")
  }
  lengths <- as.logical(length(q)) +
    as.logical(length(location)) +
    as.logical(length(scale)) +
    as.logical(length(shape))
  if (lengths >= 4) {
    nas <- any(is.na(q), is.na(location), is.na(scale), is.na(shape))
    if (!nas) chk_compatible_lengths(q, location, scale, shape)
  }
  character <- any(
    is.character(q),
    is.character(location),
    is.character(scale),
    is.character(shape)
  )
  if (lengths < 4 && !character) {
    return(vector(mode = "numeric"))
  }
  chk_false(character)
  na_shape <- is.na(shape)
  shape[na_shape] <- 0
  p <- mapply(sn::psn, x = q, xi = location, omega = scale, alpha = shape)
  p[na_shape] <- NA_real_
  p
}

#' @rdname skewnorm
#' @export
qskewnorm <- function(p, location = 0, scale = 1, shape = 0, ...,
                      mean, sd) {
  rlang::check_installed("sn")
  if (!missing(mean)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "qskewnorm(mean)",
                              id = "qskewnorm location",
                              with = "qskewnorm(location)")
    location <- mean
  }
  if (!missing(sd)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "qskewnorm(sd)",
                              id = "qskewnorm scale",
                              with = "qskewnorm(scale)")
    scale <- sd
  }
  chk_unused(...)
  chk_gte(p)
  chk_gte(scale)
  chk_lte(p, 1)
  nulls <- any(is.null(p), is.null(location), is.null(scale), is.null(shape))
  if (nulls) {
    stop("invalid arguments")
  }
  lengths <- as.logical(length(p)) +
    as.logical(length(location)) +
    as.logical(length(scale)) +
    as.logical(length(shape))
  if (lengths >= 4) {
    nas <- any(is.na(p), is.na(location), is.na(scale), is.na(shape))
    if (!nas) chk_compatible_lengths(p, location, scale, shape)
  }
  character <- any(
    is.character(p),
    is.character(location),
    is.character(scale),
    is.character(shape)
  )
  if (lengths < 4 && !character) {
    return(vector(mode = "numeric"))
  }
  chk_false(character)
  na_shape <- is.na(shape)
  shape[na_shape] <- 0
  na_scale <- is.na(scale)
  scale[na_scale] <- 0.1
  q <- mapply(sn::qsn, p = p, xi = location, omega = scale, alpha = shape)
  q[na_shape] <- NA_real_
  q[na_scale] <- NA_real_
  q
}

#' @rdname skewnorm
#' @export
rskewnorm <- function(n = 1, location = 0, scale = 1, shape = 0, ...,
                      mean, sd) {
  rlang::check_installed("sn")
  if (!missing(mean)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "rskewnorm(mean)",
                              id = "rskewnorm location",
                              with = "rskewnorm(location)")
    location <- mean
  }
  if (!missing(sd)) {
    lifecycle::deprecate_warn(when = "0.10.1", what = "rskewnorm(sd)",
                              id = "rskewnorm scale",
                              with = "rskewnorm(scale)")
    scale <- sd
  }
  chk_unused(...)
  chk_gte(n)
  chk_lt(n, Inf)
  chk_not_any_na(n)
  chk_gte(scale)
  nulls <- any(is.null(n), is.null(location), is.null(scale), is.null(shape))
  if (nulls) {
    stop("invalid arguments")
  }
  lengths <- as.logical(length(n)) +
    as.logical(length(location)) +
    as.logical(length(scale)) +
    as.logical(length(shape))
  character <- any(
    is.character(n),
    is.character(location),
    is.character(scale),
    is.character(shape)
  )
  if (lengths < 4 && !character) {
    return(vector(mode = "numeric"))
  }
  chk_whole_number(n)
  if (lengths >= 4 && n != 0L) {
    nas <- any(is.na(n), is.na(location), is.na(scale), is.na(shape))
    if (!nas) {
      chk_compatible_lengths(rep(1, n), location, scale, shape)
    }
  }
  chk_false(character)
  ran <- sn::rsn(n, xi = location, omega = scale, alpha = shape)
  attributes(ran) <- NULL
  if (n == 0L) {
    return(ran)
  }
  ran[1:n]
}
