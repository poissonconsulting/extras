#' @importFrom universals ndims
#' @export
universals::ndims

#' @inherit universals::ndims
#' @export
#' @examples 
#' ndims(character(0))
#' ndims(1:3)
ndims.default <- function(x, ...) {
  length(dims(x))
}

#' @inherit universals::ndims
#' @details Always 2L.
#' @export
#' @examples 
#' ndims(matrix(1))
ndims.matrix <- function(x, ...) {
  2L
}

#' @inherit universals::ndims
#' @details Always 2L.
#' @export
#' @examples 
#' ndims(data.frame())
ndims.data.frame <- function(x, ...) {
  2L
}
