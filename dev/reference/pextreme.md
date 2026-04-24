# Extreme Probability

Calculates the probability that a cumulative distribution function
probability is at least that extreme. **\[deprecated\]**

## Usage

``` r
pextreme(x)
```

## Arguments

- x:

  A numeric vector of values between 0 and 1.

## Value

A numeric vector of values between 0 and 1.

## See also

Other residuals:
[`sextreme()`](https://poissonconsulting.github.io/extras/dev/reference/sextreme.md)

## Examples

``` r
pextreme(seq(0, 1, by = 0.1))
#> Warning: `pextreme()` was deprecated in extras 0.1.1.
#>  [1] 0.0 0.2 0.4 0.6 0.8 1.0 0.8 0.6 0.4 0.2 0.0
```
