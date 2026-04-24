# Proportional Change

Calculates the proportional change for two sets of numbers.

## Usage

``` r
proportional_change(x, x2)
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
[`proportional_difference()`](https://poissonconsulting.github.io/extras/dev/reference/proportional_difference.md)

## Examples

``` r
proportional_change(1, 2)
#> [1] 1
proportional_change(2, 1)
#> [1] -0.5
```
