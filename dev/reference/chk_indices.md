# Check Indices

Checks if an object is a list of indices ie vectors of one or more
positive integer values.

## Usage

``` r
chk_indices(x, x_name = NULL)

vld_indices(x)
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

- `vld_indices()`: Validate Indices

## Examples

``` r
x <- list(c(2L, 1L))
chk_indices(x)
y <- c(2L, 1L)
try(chk_indices(y))
#> Error in chk_indices(y) : `y` must be a list.
vld_indices(c(3L, 1L))
#> [1] FALSE
vld_indices(list(c(3L, 1L)))
#> [1] TRUE
```
