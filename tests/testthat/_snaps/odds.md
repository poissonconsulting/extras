# `odds<-` errors if x is not numeric

    Code
      x <- NULL
      odds(x) <- "2"
    Condition
      Error in `odds<-`:
      ! `value` must be numeric.

# `odds<-` errors if x is negative

    Code
      x <- NULL
      odds(x) <- -1
    Condition
      Error in `odds<-`:
      ! `value` must be greater than or equal to 0, not -1.

