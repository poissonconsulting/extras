.onLoad <- function(libname, pkgname) {
  if (requireNamespace("memoise", quietly = TRUE)) {
    lgamma_size_x <<- memoise::memoise(lgamma_size_x)
  }
}
