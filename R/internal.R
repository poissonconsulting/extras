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
#
# Callers must reject NA `group` values (chk_not_any_na(group)) before
# calling this: unlike `size`/`prob` (attributes of an already-identified
# trial, where a missing value leaves that one trial undefined -- see
# multinom_row_na()), `group` is what identifies the trial in the first
# place. There's no sensible way to know which trial (if any) an unlabelled
# row belongs to, so it can't be resolved by attributing NA to "the trial it
# belongs to" the way size/prob can. That check lives in each calling
# function rather than here so it runs before `group` is used to split the
# data below.
#
# The size/prob checks compare only the non-NA values within a group, so a
# merely-missing value doesn't trigger a spurious error -- it's handled by
# multinom_row_na() instead, which marks the whole group's output NA rather
# than erroring (see its own comment for why).
#
# Also compares every group's row count (its number of categories) against
# the most common row count across all groups in the call, erroring on any
# that differ. This is specifically for ordinary multinomial logistic
# regression, where every trial has the same fixed set of possible
# categories: a group with fewer rows than the rest of the dataset is
# almost always a trial silently missing a row, which would otherwise be
# validated/simulated/scored as if it were a genuinely smaller, complete
# trial, giving a wrong (not NA, not an error) answer.
chk_multinom_group <- function(size, prob, group) {
  groups <- split(seq_along(group), group)
  for (idx in groups) {
    if (length(idx) < 2L) {
      stop(
        "Each `group` must contain at least 2 rows (a multinomial trial needs at least 2 categories); found a group with only 1 row. This usually means `group` lost the rows that should share a trial before reaching here.",
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

# Returns, for each row, whether its multinomial trial's draw/score can't be
# computed because `size`/`prob` is NA for ANY row sharing its group. A
# multinomial trial is one joint computation across all of its rows (the
# categories aren't independent), so missing information anywhere in that
# trial leaves the whole trial's result undefined -- not just the row that
# happens to be missing a value. `group` itself can't be NA (rejected by
# chk_multinom_group()), so every row here belongs to a real, known trial.
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
