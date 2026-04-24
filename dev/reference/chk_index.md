# Check Index

Checks if an object is a vector of one or more positive integer values.

## Usage

``` r
chk_index(x, x_name = NULL)

vld_index(x)
```

## Arguments

- x:

  An object.

- x_name:

  A string of the name of object x or NULL.

## Value

The `chk_` function throws an informative error if the test fails.

The `vld_` function returns a flag indicating whether the test was met.

## Functions

- `vld_index()`: Validate Index

## Examples

``` r
x <- c(2L, 1L)
chk_index(x)
y <- c(2L, -1L)
try(chk_index(y))
#> Error in chk_index(y) : `y` must have values greater than 0.
vld_index(c(-1))
#> [1] FALSE
vld_index(c(3L, 1L))
#> [1] TRUE
```
