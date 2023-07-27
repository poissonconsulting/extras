#' Skew-Normal Distribution
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#' @param shape A numeric vector of values.
#'
#' @return `dskewnorm` gives the density, `pskewnorm` gives the distribution function, `qskewnorm` gives the quantile function, and `rskewnorm` generates random deviates.
#' `pskewnorm` and `qskewnorm` use the lower tail probability.
#' @family skewnorm
#' @rdname skewnorm
#' @export
#'
#' @examples
#' dskewnorm(x = -2:2, mean = 0, sd = 1, shape = 0.1)
#' dskewnorm(x = -2:2, mean = 0, sd = 1, shape = -1)
#' qskewnorm(p = c(0.1, 0.4), mean = 0, sd = 1, shape = 0.1)
#' qskewnorm(p = c(0.1, 0.4), mean = 0, sd = 1, shape = -1)
#' pskewnorm(q = -2:2, mean = 0, sd = 1, shape = 0.1)
#' pskewnorm(q = -2:2, mean = 0, sd = 1, shape = -1)
#' rskewnorm(n = 3, mean = 0, sd = 1, shape = 0.1)
#' rskewnorm(n = 3, mean = 0, sd = 1, shape = -1)
dskewnorm <- function(x, mean = 0, sd = 1, shape = 0, log = FALSE) {
  if (!requireNamespace("sn", quietly = TRUE)) {
    stop(
      "Package \"sn\" must be installed to use this function.",
      call. = FALSE
    )
  }
  chk_gte(sd)
  nulls <- any(is.null(x), is.null(mean), is.null(sd), is.null(shape))
  if (nulls) stop("invalid arguments")
  lengths <- as.logical(length(x)) + as.logical(length(mean)) + as.logical(length(sd)) + as.logical(length(shape))
  if (lengths >= 4) {
    nas <- any(is.na(x), is.na(mean), is.na(sd), is.na(shape))
    if (!nas) chk_compatible_lengths(x, mean, sd, shape)
  }
  character <- any(is.character(x), is.character(mean), is.character(sd), is.character(shape))
  if (lengths < 4 & !character) {
    return(vector(mode = "numeric"))
  }
  chk_false(character)
  na_shape <- is.na(shape)
  shape[na_shape] <- 0
  lik <- sn::dsn(x = x, xi = mean, omega = sd, alpha = shape, log = log)
  lik[na_shape] <- NA_real_
  lik
}

#' @rdname skewnorm
#' @export
pskewnorm <- function(q, mean = 0, sd = 1, shape = 0) {
  if (!requireNamespace("sn", quietly = TRUE)) {
    stop(
      "Package \"sn\" must be installed to use this function.",
      call. = FALSE
    )
  }
  chk_gte(sd)
  nulls <- any(is.null(q), is.null(mean), is.null(sd), is.null(shape))
  if (nulls) stop("invalid arguments")
  lengths <- as.logical(length(q)) + as.logical(length(mean)) + as.logical(length(sd)) + as.logical(length(shape))
  if (lengths >= 4) {
    nas <- any(is.na(q), is.na(mean), is.na(sd), is.na(shape))
    if (!nas) chk_compatible_lengths(q, mean, sd, shape)
  }
  character <- any(is.character(q), is.character(mean), is.character(sd), is.character(shape))
  if (lengths < 4 & !character) {
    return(vector(mode = "numeric"))
  }
  chk_false(character)
  na_shape <- is.na(shape)
  shape[na_shape] <- 0
  p <- mapply(sn::psn, x = q, xi = mean, omega = sd, alpha = shape)
  p[na_shape] <- NA_real_
  p
}

#' @rdname skewnorm
#' @export
qskewnorm <- function(p, mean = 0, sd = 1, shape = 0) {
  if (!requireNamespace("sn", quietly = TRUE)) {
    stop(
      "Package \"sn\" must be installed to use this function.",
      call. = FALSE
    )
  }
  chk_gte(sd)
  chk_gte(p)
  chk_lte(p, 1)
  nulls <- any(is.null(p), is.null(mean), is.null(sd), is.null(shape))
  if (nulls) stop("invalid arguments")
  lengths <- as.logical(length(p)) + as.logical(length(mean)) + as.logical(length(sd)) + as.logical(length(shape))
  if (lengths >= 4) {
    nas <- any(is.na(p), is.na(mean), is.na(sd), is.na(shape))
    if (!nas) chk_compatible_lengths(p, mean, sd, shape)
  }
  character <- any(is.character(p), is.character(mean), is.character(sd), is.character(shape))
  if (lengths < 4 & !character) {
    return(vector(mode = "numeric"))
  }
  chk_false(character)
  na_shape <- is.na(shape)
  shape[na_shape] <- 0
  na_sd <- is.na(sd)
  sd[na_sd] <- 0.1
  q <- mapply(sn::qsn, p = p, xi = mean, omega = sd, alpha = shape)
  q[na_shape] <- NA_real_
  q[na_sd] <- NA_real_
  q
}

#' @rdname skewnorm
#' @export
rskewnorm <- function(n = 1, mean = 0, sd = 1, shape = 0) {
  if (!requireNamespace("sn", quietly = TRUE)) {
    stop(
      "Package \"sn\" must be installed to use this function.",
      call. = FALSE
    )
  }
  chk_gte(n)
  chk_lt(n, Inf)
  chk_not_any_na(n)
  chk_gte(sd)
  nulls <- any(is.null(n), is.null(mean), is.null(sd), is.null(shape))
  if (nulls) stop("invalid arguments")
  lengths <- as.logical(length(n)) + as.logical(length(mean)) + as.logical(length(sd)) + as.logical(length(shape))
  character <- any(is.character(n), is.character(mean), is.character(sd), is.character(shape))
  if (lengths < 4 & !character) {
    return(vector(mode = "numeric"))
  }
  chk_whole_number(n)
  if (lengths >= 4 & n != 0L) {
    nas <- any(is.na(n), is.na(mean), is.na(sd), is.na(shape))
    if (!nas) {
      chk_compatible_lengths(rep(1, n), mean, sd, shape)
    }
  }
  chk_false(character)
  ran <- sn::rsn(n, xi = mean, omega = sd, alpha = shape)
  attributes(ran) <- NULL
  if (n == 0L) return(ran)
  ran[1:n]
}
