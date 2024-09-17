#' Numericise (or Numericize)
#'
#' Coerce an R object to a numeric atomic object.
#'
#' @inheritParams params
#' @return A numeric atomic object.
#' @aliases numericize
#' @export
numericise <- function(x, ...) UseMethod("numericise")

#' @rdname numericise
#' @details
#' `numericize()` is an alias for numericise.
#' If you want to implement a method for a class `"foo"`, implement
#' `numericise.foo()`.
#'
#' @export
numericize <- function(x, ...) UseMethod("numericise")

#' @describeIn numericise Numericise a logical Object
#' @export
#' @examples
#'
#' # logical
#' numericise(TRUE)
#' numericise(matrix(c(TRUE, FALSE), nrow = 2))
numericise.logical <- function(x, ...) as.integer(x)

#' @describeIn numericise Numericise an integer Object
#' @export
#' @examples
#'
#' # integer
#' numericise(2L)
numericise.integer <- function(x, ...) x

#' @describeIn numericise Numericise an double Object
#' @export
#' @examples
#'
#' # double
#' numericise(c(1, 3))
numericise.double <- function(x, ...) x

#' @describeIn numericise Numericise a factor
#' @export
#' @examples
#'
#' # factor
#' numericise(factor(c("c", "a")))
numericise.factor <- function(x, ...) as.integer(x)

#' @describeIn numericise Numericise a Date vector
#' @export
#' @examples
#'
#' # Date
#' numericise(as.Date("1972-01-01"))
numericise.Date <- function(x, ...) {
  x <- unclass(x)
  as.numeric(x)
}

#' @describeIn numericise Numericise a POSIXct vector
#' @export
#' @examples
#'
#' # POSIXct
#' numericise(as.POSIXct("1972-01-01", tz = "UTC"))
numericise.POSIXct <- function(x, ...) {
  x <- unclass(x)
  as.numeric(x)
}

#' @describeIn numericise Numericise a hms vector
#' @export
#' @examples
#'
#' # hms
#' @examplesIf requireNamespace("hms")
#' numericise(hms::as_hms("00:01:03"))
numericise.hms <- function(x, ...) {
  rlang::check_installed("hms")
  x <- unclass(x)
  as.numeric(x)
}

#' @describeIn numericise Numericise a matrix
#' @export
#' @examples
#'
#' # matrix
#' numericise(matrix(TRUE))
numericise.matrix <- function(x, ...) {
  if (is.logical(x)) {
    mode(x) <- "integer"
    return(x)
  }
  dim <- dim(x)
  x <- numericize(as.vector(x))
  dim(x) <- dim
  x
}

#' @describeIn numericise Numericise an array
#' @export
#' @examples
#'
#' # array
#' numericise(array(TRUE))
numericise.array <- function(x, ...) {
  if (is.logical(x)) {
    mode(x) <- "integer"
    return(x)
  }
  dim <- dim(x)
  x <- numericize(as.vector(x))
  dim(x) <- dim
  x
}

#' @describeIn numericise Numericise a data.frame
#' @export
#' @examples
#'
#' # data.frame
#' numericise(data.frame(
#'   logical = c(TRUE, FALSE, NA),
#'   integer = 1:3,
#'   numeric = c(4, 10, NA),
#'   factor = as.factor(c("c", "A", "green"))
#' ))
numericise.data.frame <- function(x, ...) {
  x[] <- lapply(x, numericise)
  x <- as.matrix(x)
  x
}
