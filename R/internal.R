dev_res <- function(x, mu, dev) {
  sign(x - mu) * sqrt(dev)
}

# Validates that every row sharing a `group` (multinomial trial) has the
# same `size` and that its `prob` values sum to 1, since both the
# Poisson-equivalent deviance/log-likelihood identities and simulation via
# rmultinom() require this to hold.
chk_multinom_group <- function(size, prob, group) {
  for (idx in split(seq_along(group), group)) {
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
