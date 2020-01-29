nchains.integer <- function(x, ...) {
  2L
}

nsims.integer <- function(x, ...) {
  2L
}

nterms.integer <- function(x, ...) {
  length(x)
}

pars.character <- function(x, scalar = NA, ...) {
  chk_unused(...)
  x <- unique(x)
  if(vld_true(scalar)) return(x[x == "scalar"])
  if(vld_false(scalar)) return(x[x != "scalar"])
  x
}

pdims.default <- function(x, scalar = NA, ...) {
  chk_unused(...)
  if(vld_true(scalar)) return(list(scalar = c(2L, 1L)))
  if(vld_false(scalar)) return(list(vector = 3L))
  return(list(scalar = c(2L, 1L), vector = 3L))
}

pdims_terms.default <- function(x, scalar = NA, ...) {
  chk_unused(...)
  if(vld_true(scalar)) return(list(scalar = c(2L, 1L)))
  if(vld_false(scalar)) return(list(vector = 3L))
  return(list(scalar = c(2L, 1L), vector = 3L))
}

set_pars.character <- function(x, value) {
  chk_identical(length(value), length(x))
  value
}
