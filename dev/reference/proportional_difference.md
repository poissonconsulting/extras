# Proportional Difference

Calculates the proportional difference for two sets of numbers.

## Usage

``` r
proportional_difference(x, x2)
```

## Arguments

- x:

  A numeric object (vector, matrix or array) of non-negative numbers.

- x2:

  A second numeric object of non-negative numbers.

## Value

A numeric object of the proportional change.

## See also

Other proportional:
[`proportional_change()`](https://poissonconsulting.github.io/extras/dev/reference/proportional_change.md)

## Examples

``` r
proportional_difference(1, 2)
#> [1] 0.6666667
proportional_difference(2, 1)
#> [1] -0.6666667
```
