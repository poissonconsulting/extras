# Odds Ratio2

Calculates the odds ratio for a vector of two probabilities.

## Usage

``` r
odds_ratio2(x)
```

## Arguments

- x:

  A numeric vector of length 2.

## Value

A number.

## See also

Other odds fun2:
[`log_odds_ratio2()`](https://poissonconsulting.github.io/extras/dev/reference/log_odds_ratio2.md)

## Examples

``` r
odds_ratio2(c(0.5, 0.9))
#> [1] 0.1111111
odds_ratio2(c(0.9, 0.5))
#> [1] 9
```
