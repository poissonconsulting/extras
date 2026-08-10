dev_res <- function(x, mu, dev) {
  sign(x - mu) * sqrt(dev)
}

# Checks every group shares one `size` and `prob` values summing to 1
# (required by rmultinom() and the deviance/log-lik identities), has >= 2
# rows (a trial needs >= 2 categories -- singletons usually mean `group`
# was evaluated row-by-row instead of over the whole vector), and matches
# the modal row count across groups (a short group usually means a row was
# lost). Only non-NA values are compared, so lone NAs don't error here --
# see multinom_row_na(). Callers must chk_not_any_na(group) first; `group`
# itself can't be NA-tolerant since it's what identifies the trial.
chk_multinom_group <- function(size, prob, group) {
  groups <- split(seq_along(group), group)
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
    prob_sum <- sum(prob[idx])
    if (!is.na(prob_sum) && abs(prob_sum - 1) > 1e-6) {
      stop(
        "`prob` must sum to 1 for every `group` (multinomial trial).",
        call. = FALSE
      )
    }
  }
  if (length(groups) > 1L) {
    group_sizes <- lengths(groups)
    size_counts <- table(group_sizes)
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
multinom_row_na <- function(size, prob, group) {
  bad <- is.na(size) | is.na(prob)
  result <- rep(FALSE, length(group))
  for (idx in split(seq_along(group), group)) {
    if (any(bad[idx])) {
      result[idx] <- TRUE
    }
  }
  result
}
