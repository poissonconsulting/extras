# Log Odds

Calculates the log odds for probabilities.

## Usage

``` r
log_odds(x)
```

## Arguments

- x:

  A numeric object (vector, matrix or array) of probabilities.

## Value

A numeric object of the the log odds for each probability.

## See also

Other odds:
[`inv_odds()`](https://poissonconsulting.github.io/extras/dev/reference/inv_odds.md),
`log_odds<-()`,
[`log_odds_ratio()`](https://poissonconsulting.github.io/extras/dev/reference/log_odds_ratio.md),
[`odds()`](https://poissonconsulting.github.io/extras/dev/reference/odds.md),
`odds<-()`,
[`odds_ratio()`](https://poissonconsulting.github.io/extras/dev/reference/odds_ratio.md)

## Examples

``` r
log_odds(c(0, 0.5, 0.9, 1))
#> [1]     -Inf 0.000000 2.197225      Inf
```
