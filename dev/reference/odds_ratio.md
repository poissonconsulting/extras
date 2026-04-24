# Odds Ratio

Calculates the odds ratio for two probabilities.

## Usage

``` r
odds_ratio(x, x2)
```

## Arguments

- x:

  A numeric object (vector, matrix or array) of probabilities.

- x2:

  A second numeric object of probabilities.

## Value

A numeric object of the odds ratios.

## See also

Other odds:
[`inv_odds()`](https://poissonconsulting.github.io/extras/dev/reference/inv_odds.md),
[`log_odds()`](https://poissonconsulting.github.io/extras/dev/reference/log_odds.md),
`log_odds<-()`,
[`log_odds_ratio()`](https://poissonconsulting.github.io/extras/dev/reference/log_odds_ratio.md),
[`odds()`](https://poissonconsulting.github.io/extras/dev/reference/odds.md),
`odds<-()`

## Examples

``` r
odds_ratio(0.5, 0.75)
#> [1] 0.3333333
```
