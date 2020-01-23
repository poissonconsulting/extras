nchains.integer <- function(x, ...) {
  2L
}

nsims.integer <- function(x, ...) {
  2L
}

nterms.integer <- function(x, ...) {
  length(x)
}

pars.character <- function(x, ...) {
  unique(x)
}

set_pars.character <- function(x, value) {
  chk_identical(length(value), length(x))
  value
}
