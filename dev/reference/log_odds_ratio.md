# Log-Odds Ratio

Calculates the log odds ratio for two probabilities.

## Usage

``` r
log_odds_ratio(x, x2)
```

## Arguments

- x:

  A numeric object (vector, matrix or array) of probabilities.

- x2:

  A second numeric object of probabilities.

## Value

A numeric object of the log odds ratios.

## See also

Other odds:
[`inv_odds()`](https://poissonconsulting.github.io/extras/dev/reference/inv_odds.md),
[`log_odds()`](https://poissonconsulting.github.io/extras/dev/reference/log_odds.md),
`log_odds<-()`,
[`odds()`](https://poissonconsulting.github.io/extras/dev/reference/odds.md),
`odds<-()`,
[`odds_ratio()`](https://poissonconsulting.github.io/extras/dev/reference/odds_ratio.md)

## Examples

``` r
log_odds_ratio(0.5, 0.75)
#> [1] -1.098612
```
