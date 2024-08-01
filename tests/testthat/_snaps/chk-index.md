# chk_index errors with x = NA

    Code
      chk_index(NA)
    Condition
      Error in `chk_index()`:
      ! `NA` must be integer.

# chk_index errors with empty x

    Code
      chk_index(integer(0))
    Condition
      Error in `chk_index()`:
      ! `integer(0)` must not be empty (zero length).

