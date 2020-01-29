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

set_pars.character <- function(x, value) {
  chk_identical(length(value), length(x))
  value
}
