#' @importFrom universals ndims
#' @export
universals::ndims

#' Number of Dimensions of an Object
#' 
#' @inherit universals::ndims
#' @export
#' @examples 
#' ndims(character(0))
#' ndims(1:3)
#' @export
ndims.default <- function(x, ...) {
  length(dims(x))
}

#' Dimensions of a Matrix
#' 
#' @inherit universals::ndims
#'
#' @details Always 2L.
#' @export
#' @examples 
#' ndims(matrix(1))
ndims.matrix <- function(x, ...) {
  2L
}

#' Dimensions of a Data Frame
#' 
#' @inherit universals::ndims
#'
#' @details Always 2L.
#' @export
#' @examples 
#' ndims(data.frame())
ndims.data.frame <- function(x, ...) {
  2L
}
