# A deviance is analytically non-negative but can compute as a tiny negative
# number when mu equals x to machine precision, which sqrt() turns into NaN.
dev_res <- function(x, mu, dev) {
  dev[!is.na(dev) & dev < 0 & dev > -sqrt(.Machine$double.eps)] <- 0
  sign(x - mu) * sqrt(dev)
}

# Row indices for each `group`, shared across the multinom_* helpers within
# a single call (chk_multinom_group(), multinom_row_na(), the sampling loop
# in ran_multinom()) so `group` isn't re-split by every one of them.
multinom_split <- function(group) {
  split(seq_along(group), group)
}

# Checks every group shares one `size` and `prob` values summing to 1
# (required by rmultinom() and the deviance/log-lik identities), has >= 2
# rows (a trial needs >= 2 categories -- singletons usually mean `group`
# was evaluated row-by-row instead of over the whole vector), and matches
# the modal row count across groups (a short group usually means a row was
# lost). Only non-NA values are compared, so lone NAs don't error here --
# see multinom_row_na(). Callers must chk_not_any_na(group) first; `group`
# itself can't be NA-tolerant since it's what identifies the trial.
chk_multinom_group <- function(
  size,
  prob,
  group,
  groups = multinom_split(group)
) {
  for (idx in groups) {
    if (length(idx) < 2L) {
      stop(
        "Each `group` must contain at least 2 rows (a multinomial trial needs at least 2 categories); found a group with only 1 row. This usually means `group`/`size`/`prob` were passed one row at a time instead of as vectors.",
        call. = FALSE
      )
    }
    known_size <- size[idx][!is.na(size[idx])]
    if (length(unique(known_size)) > 1L) {
      stop(
        "`size` must be the same for every row belonging to the same `group` (multinomial trial).",
        call. = FALSE
      )
    }
    known_prob <- prob[idx][!is.na(prob[idx])]
    known_prob_sum <- sum(known_prob)
    # a group with a missing prob can only be validated one-sided: the known
    # values must not already exceed 1, since a full sum-to-1 check would be
    # (wrongly) skipped whenever any prob in the group is NA
    prob_bad <- if (length(known_prob) < length(idx)) {
      known_prob_sum > 1 + 1e-6
    } else {
      abs(known_prob_sum - 1) > 1e-6
    }
    if (prob_bad) {
      stop(
        "`prob` must sum to 1 for every `group` (multinomial trial).",
        call. = FALSE
      )
    }
  }
  if (length(groups) > 1L) {
    group_sizes <- lengths(groups)
    size_counts <- table(group_sizes)
    # ties are broken in favour of the smallest row count (table()'s names
    # are sorted ascending, and which.max() takes the first maximum)
    mode_size <- as.integer(names(size_counts)[which.max(size_counts)])
    bad <- group_sizes != mode_size
    if (any(bad)) {
      stop(
        sprintf(
          "Every `group` should have the same number of rows (%d, the most common number of categories in this data); found a group (\"%s\") with %d row(s) instead. This usually means `group` lost a row that should have been part of that trial.",
          mode_size,
          names(groups)[bad][1],
          group_sizes[bad][1]
        ),
        call. = FALSE
      )
    }
  }
}

# Flags every row whose trial has an NA `size`/`prob` anywhere in the group,
# since a trial's categories are scored/drawn jointly, not independently.
multinom_row_na <- function(size, prob, group, groups = multinom_split(group)) {
  bad <- is.na(size) | is.na(prob)
  result <- rep(FALSE, length(group))
  for (idx in groups) {
    if (any(bad[idx])) {
      result[idx] <- TRUE
    }
  }
  result
}

# Gauss-Hermite nodes and weights for integrals of the form
# integral(f(x) * exp(-x^2)) by the Golub-Welsch algorithm.
gauss_hermite <- function(n) {
  jacobi <- matrix(0, n, n)
  off <- sqrt(seq_len(n - 1) / 2)
  jacobi[cbind(seq_len(n - 1), 2:n)] <- off
  jacobi[cbind(2:n, seq_len(n - 1))] <- off
  e <- eigen(jacobi, symmetric = TRUE)
  list(x = e$values, w = sqrt(pi) * e$vectors[1, ]^2)
}

# The 10-node rule used by the Poisson-lognormal quadrature, computed once at
# build time. `lw` holds log(w) + x^2 so that the nodes can be applied to an
# integrand that already includes the standard normal density.
pois_lnorm_gh <- local({
  gh <- gauss_hermite(10)
  list(x = gh$x, lw = log(gh$w) + gh$x^2)
})

# Mode of the log integrand of the Poisson-lognormal over the standard normal
# deviate z, i.e. the root of x * sdlog - sdlog * lam(z) - z where
# lam(z) = lambda * exp(sdlog * z - sdlog^2 / 2). The function is concave and
# decreasing in z, so Newton steps from the right of the root converge
# monotonically. Starting points left of the root are moved right by one
# Newton step (which overshoots the root) and then capped at an upper bound
# whose lam(z) is max(x + 0.5, lambda), which keeps exp() finite.
# Requires x >= 0, lambda > 0 and sdlog > 0, all finite and of equal length.
pois_lnorm_mode <- function(x, lambda, sdlog, tol = 1e-10, maxit = 200L) {
  score <- function(z, i) {
    x[i] *
      sdlog[i] -
      sdlog[i] * lambda[i] * exp(sdlog[i] * z - sdlog[i]^2 / 2) -
      z
  }
  slope <- function(z, i) {
    -sdlog[i]^2 * lambda[i] * exp(sdlog[i] * z - sdlog[i]^2 / 2) - 1
  }
  all <- seq_along(x)
  z <- sdlog * (log((x + 0.5) / lambda) + sdlog^2 / 2) / (1 + sdlog^2)
  left <- which(score(z, all) > 0)
  z[left] <- z[left] - score(z[left], left) / slope(z[left], left)
  z_upper <- (log(pmax(x + 0.5, lambda) / lambda) + sdlog^2 / 2) / sdlog
  z <- pmin(z, z_upper)
  active <- all
  for (i in seq_len(maxit)) {
    if (!length(active)) {
      break
    }
    za <- z[active]
    step <- -score(za, active) / slope(za, active)
    z[active] <- za + step
    active <- active[abs(step) > tol * pmax(1, abs(za))]
  }
  z
}

# Marginal log-likelihood of the Poisson-lognormal by adaptive Gauss-Hermite
# quadrature centred on the mode of the integrand and scaled by its curvature.
# With `moments = TRUE` also returns the posterior mean and variance of the
# Poisson rate given x, which are used to find the saturated lambda.
# Requires x >= 0 whole, lambda > 0 and sdlog > 0, all finite and of equal
# length.
pois_lnorm_quad <- function(x, lambda, sdlog, moments = FALSE) {
  nodes <- length(pois_lnorm_gh$x)
  z <- pois_lnorm_mode(x, lambda, sdlog)
  lam <- lambda * exp(sdlog * z - sdlog^2 / 2)
  sd <- 1 / sqrt(sdlog^2 * lam + 1)
  zk <- z + outer(sqrt(2) * sd, pois_lnorm_gh$x)
  lamk <- lambda * exp(sdlog * zk - sdlog^2 / 2)
  log_node <- stats::dpois(x, lamk, log = TRUE) +
    stats::dnorm(zk, log = TRUE) +
    rep(pois_lnorm_gh$lw, each = length(x))
  # Bounds every node so exp() cannot overflow.
  offset <- stats::dpois(x, lam, log = TRUE) +
    stats::dnorm(z, log = TRUE) +
    max(pois_lnorm_gh$lw)
  weight <- exp(log_node - offset)
  sum_weight <- rowSums(weight)
  log_lik <- offset + log(sum_weight) + log(sqrt(2) * sd)
  if (!moments) {
    return(log_lik)
  }
  mean <- rowSums(weight * lamk) / sum_weight
  var <- rowSums(weight * lamk^2) / sum_weight - mean^2
  list(log_lik = log_lik, mean = mean, var = var)
}

# Recycles the arguments of a pois_lnorm function to a common length following
# the base R d/p/q functions (length zero if any argument has length zero).
pois_lnorm_recycle <- function(x, lambda, sdlog) {
  n <- if (length(x) && length(lambda) && length(sdlog)) {
    max(length(x), length(lambda), length(sdlog))
  } else {
    0L
  }
  list(
    x = rep_len(as.numeric(x), n),
    lambda = rep_len(as.numeric(lambda), n),
    sdlog = rep_len(as.numeric(sdlog), n)
  )
}

# Whether the Poisson-lognormal quadrature applies: x finite non-negative whole,
# lambda finite positive and sdlog finite positive. All other values are
# handled by the Poisson distribution or as limiting cases.
pois_lnorm_use_quad <- function(x, lambda, sdlog) {
  !is.na(x) &
    !is.na(lambda) &
    !is.na(sdlog) &
    is.finite(x) &
    x >= 0 &
    x == floor(x) &
    is.finite(lambda) &
    lambda > 0 &
    is.finite(sdlog) &
    sdlog > 0
}

# Lambda that maximises the Poisson-lognormal marginal log-likelihood of x > 0
# at fixed sdlog. The maximum is where the posterior mean of the Poisson rate
# equals x and lies between x and x * exp(sdlog^2 / 2) (the large x limit).
# Safeguarded Newton steps on log(lambda) fall back to bisection when a step
# leaves the bracket. Requires x > 0 whole and sdlog > 0, both finite and of
# equal length.
pois_lnorm_saturated <- function(x, sdlog, tol = 1e-10, maxit = 100L) {
  lower <- log(x)
  upper <- log(x) + sdlog^2 / 2
  eta <- log(x) + sdlog^2 / 2 * x / (x + 1)
  active <- seq_along(x)
  for (i in seq_len(maxit)) {
    if (!length(active)) {
      break
    }
    quad <- pois_lnorm_quad(
      x[active],
      exp(eta[active]),
      sdlog[active],
      moments = TRUE
    )
    score <- x[active] - quad$mean
    slope <- quad$var - quad$mean
    pos <- score > 0
    lower[active][pos] <- eta[active][pos]
    upper[active][!pos] <- eta[active][!pos]
    new <- eta[active] - score / slope
    bisect <- !is.finite(new) |
      slope >= 0 |
      new <= lower[active] |
      new >= upper[active]
    new[bisect] <- (lower[active][bisect] + upper[active][bisect]) / 2
    step <- abs(new - eta[active])
    eta[active] <- new
    active <- active[step > tol & upper[active] - lower[active] > tol]
  }
  eta
}
