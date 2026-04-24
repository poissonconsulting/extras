# Extreme Surprisal

Calculates the surprisal (in bits) that a cumulative distribution
function probability is at least that extreme. **\[deprecated\]**

## Usage

``` r
sextreme(x, directional = FALSE)
```

## Arguments

- x:

  A numeric vector of values between 0 and 1.

- directional:

  A flag specifying whether probabilities less than 0.5 should be
  returned as negative values.

## Value

A numeric vector of surprisal values.

## See also

Other residuals:
[`pextreme()`](https://poissonconsulting.github.io/extras/dev/reference/pextreme.md)

## Examples

``` r
sextreme(seq(0.1, 0.9, by = 0.1))
#> [1] 2.3219281 1.3219281 0.7369656 0.3219281 0.0000000 0.3219281 0.7369656
#> [8] 1.3219281 2.3219281
sextreme(seq(0.1, 0.9, by = 0.1), directional = TRUE)
#> [1] -2.3219281 -1.3219281 -0.7369656 -0.3219281  0.0000000  0.3219281  0.7369656
#> [8]  1.3219281  2.3219281
```
