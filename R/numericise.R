#' Numericise (or Numericize)
#'
#' Coerce an R object to a numeric atomic object.
#'
#' @inheritParams params
#' @return A numeric atomic object.
#' @aliases numericize
#' @export
#' @examples
#' numericise.foobar <- function(x, ...) {
#'   NotYetImplemented()
#'   # replace with code to numericise an object of class 'foobar'
#' }
numericise <- function(x, ...) UseMethod("numericise")

#' @rdname numericise
#' @details
#' `numericize()` is an alias for numericise.
#' If you want to implement a method for a class `"foo"`, implement
#' `numericise.foo()`.
#'
#' @export
#' @examples
#' numericise.foobar <- function(x, ...) {
#'   NotYetImplemented()
#'   # replace with code to numericise an object of class 'foobar'
#' }
numericize <- function(x, ...) UseMethod("numericise")

#' @inherit numericise
#' @return An integer numeric atomic object.
#' @export
#' @examples
#' numericise(TRUE)
#' numericise(matrix(c(TRUE, FALSE), nrow = 2))
numericise.logical <- function(x, ...) as.integer(x)

#' @inherit numericise
#' @return An integer numeric atomic object.
#' @export
#' @examples
#' numericise(2L)
numericise.integer <- function(x, ...) x

#' @inherit numericise
#' @return A double numeric atomic object.
#' @export
#' @examples
#' numericise(c(1, 3))
numericise.double <- function(x, ...) x

#' @inherit numericise
#' @return A positive integer numeric atomic vector object.
#' @export
#' @examples
#' numericise(factor(c("c", "a")))
numericise.factor <- function(x, ...) as.integer(x)

#' @inherit numericise
#' @return A double numeric atomic vector object.
#' @export
#' @examples
#' numericise(as.Date("1972-01-01"))
numericise.Date <- function(x, ...) {
  x <- unclass(x)
  as.numeric(x)
}

#' @inherit numericise
#' @return A double numeric atomic vector object.
#' @export
#' @examples
#' numericise(as.POSIXct("1972-01-01", tz = "UTC"))
numericise.POSIXct <- function(x, ...) {
  x <- unclass(x)
  as.numeric(x)
}

#' @inherit numericise
#' @return A double numeric atomic matrix object.
#' @export
#' @examples
#' numericise(matrix(TRUE))
numericise.matrix <- function(x, ...) {
  if (is.logical(x)) {
    mode(x) <- "integer"
    return(x)
  }
  dims <- dims(x)
  x <- numericize(as.vector(x))
  dim(x) <- dims
  x
}

#' @inherit numericise
#' @return A double numeric atomic matrix object.
#' @export
#' @examples
#' numericise(array(TRUE))
numericise.array <- function(x, ...) {
  if (is.logical(x)) {
    mode(x) <- "integer"
    return(x)
  }
  dims <- dims(x)
  x <- numericize(as.vector(x))
  dim(x) <- dims
  x
}

#' @inherit numericise
#' @return A double numeric atomic matrix object.
#' @export
#' @examples
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

#' @inherit numericise
#' @return A double numeric atomic vector object.
#' @export
#' @examples
#' numericise(hms::as_hms("00:01:03"))
numericise.hms <- function(x, ...) {
  # I'm not sure if this test is needed since S3 function
  if (!requireNamespace("hms", quietly = TRUE)) {
    err("Please install the 'hms' package.")
  }
  x <- unclass(x)
  as.numeric(x)
}
