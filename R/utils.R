set_class <- function(x, class) {
  class(x) <- class
  x
}

set_names <- function(x, names) {
  names(x) <- names
  x
}

sys_time <- function() {
  time <- Sys.time()
  attr(time, "tzone") <- "UTC"
  time
}

is_try_error <- function(x) inherits(x, "try-error")

# from https://recology.info/2018/10/limiting-dependencies/
remove_nulls <- function(x) Filter(Negate(is.null), x)

# from https://recology.info/2018/10/limiting-dependencies/
str_extract <- function(x, y) regmatches(x, regexpr(y, x))

str_extract_all <- function(x, y) regmatches(x, gregexpr(y, x))

# from https://recology.info/2018/10/limiting-dependencies/
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

rinteger <- function(n = 1) as.integer(stats::runif(n, -.max_integer, .max_integer))

last <- function(x) x[length(x)]
