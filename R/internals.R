
# for testing
pars.character <- function(x, ...) {
  unique(x)
}

# for testing
set_pars.character <- function(x, value) {
  chk_identical(length(value), length(x))
  value
}
