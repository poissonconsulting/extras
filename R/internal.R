dev_res <- function(x, mu, dev) {
  sign(x - mu) * sqrt(dev)
}

# Validates that every row sharing a `group` (multinomial trial) has the
# same `size` and that its `prob` values sum to 1, since both the
# Poisson-equivalent deviance/log-likelihood identities and simulation via
# rmultinom() require this to hold. Also rejects singleton groups: a group
# with only one row can't represent a real multinomial trial (a trial needs
# at least 2 categories) -- rmultinom() would just deterministically return
# `size` for it. This typically happens if the function ends up being
# evaluated separately for each row instead of once for the whole data
# vector, so each call only ever sees a single row's own group value.
chk_multinom_group <- function(size, prob, group) {
  for (idx in split(seq_along(group), group)) {
    if (length(idx) < 2L) {
      stop(
        "Each `group` must contain at least 2 rows (a multinomial trial needs at least 2 categories); found a group with only 1 row. This usually means `group` lost the rows that should share a trial before reaching here.",
        call. = FALSE
      )
    }
    if (length(unique(size[idx])) > 1L) {
      stop(
        "`size` must be the same for every row belonging to the same `group` (multinomial trial).",
        call. = FALSE
      )
    }
    prob_sum <- sum(prob[idx])
    if (!is.na(prob_sum) && abs(prob_sum - 1) > 1e-6) {
      stop(
        "`prob` must sum to 1 for every `group` (multinomial trial).",
        call. = FALSE
      )
    }
  }
}
