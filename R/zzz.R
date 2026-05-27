.onLoad <- function(libname, pkgname) {
  if (rlang::is_installed("memoise")) {
    lgamma_size_x <<- memoise::memoise(lgamma_size_x)
  }
}
