# Numericise (or Numericize)

Coerce an R object to a numeric atomic object.

## Usage

``` r
numericise(x, ...)

numericize(x, ...)

# S3 method for class 'logical'
numericise(x, ...)

# S3 method for class 'integer'
numericise(x, ...)

# S3 method for class 'double'
numericise(x, ...)

# S3 method for class 'factor'
numericise(x, ...)

# S3 method for class 'Date'
numericise(x, ...)

# S3 method for class 'POSIXct'
numericise(x, ...)

# S3 method for class 'hms'
numericise(x, ...)

# S3 method for class 'matrix'
numericise(x, ...)

# S3 method for class 'array'
numericise(x, ...)

# S3 method for class 'data.frame'
numericise(x, ...)
```

## Arguments

- x:

  An object.

- ...:

  Other arguments passed to methods.

## Value

A numeric atomic object.

## Details

`numericize()` is an alias for numericise. If you want to implement a
method for a class `"foo"`, implement `numericise.foo()`.

## Methods (by class)

- `numericise(logical)`: Numericise a logical Object

- `numericise(integer)`: Numericise an integer Object

- `numericise(double)`: Numericise an double Object

- `numericise(factor)`: Numericise a factor

- `numericise(Date)`: Numericise a Date vector

- `numericise(POSIXct)`: Numericise a POSIXct vector

- `numericise(hms)`: Numericise a hms vector

- `numericise(matrix)`: Numericise a matrix

- `numericise(array)`: Numericise an array

- `numericise(data.frame)`: Numericise a data.frame

## Examples

``` r
# logical
numericise(TRUE)
#> [1] 1
numericise(matrix(c(TRUE, FALSE), nrow = 2))
#>      [,1]
#> [1,]    1
#> [2,]    0

# integer
numericise(2L)
#> [1] 2

# double
numericise(c(1, 3))
#> [1] 1 3

# factor
numericise(factor(c("c", "a")))
#> [1] 2 1

# Date
numericise(as.Date("1972-01-01"))
#> [1] 730

# POSIXct
numericise(as.POSIXct("1972-01-01", tz = "UTC"))
#> [1] 63072000

# hms
numericise(hms::as_hms("00:01:03"))
#> [1] 63

# matrix
numericise(matrix(TRUE))
#>      [,1]
#> [1,]    1

# array
numericise(array(TRUE))
#> [1] 1

# data.frame
numericise(data.frame(
  logical = c(TRUE, FALSE, NA),
  integer = 1:3,
  numeric = c(4, 10, NA),
  factor = as.factor(c("c", "A", "green"))
))
#>      logical integer numeric factor
#> [1,]       1       1       4      2
#> [2,]       0       2      10      1
#> [3,]      NA       3      NA      3
```
