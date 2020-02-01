nchains.integer <- function(x, ...) {
  2L
}

niters.integer <- function(x, ...) {
  1L
}

nterms.integer <- function(x, ...) {
  length(x)
}

pars.character <- function(x, ...) {
  chk_unused(...)
  x <- unique(x)
  x
}

pdims.default <- function(x, ...) {
  chk_unused(...)
  return(list(scalar = c(2L, 1L), vector = 3L))
}

set_pars.character <- function(x, value) {
  chk_identical(length(value), length(x))
  value
}
