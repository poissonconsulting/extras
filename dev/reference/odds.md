# Odds

Calculates the odds for probabilities.

## Usage

``` r
odds(x)
```

## Arguments

- x:

  A numeric object (vector, matrix or array) of probabilities.

## Value

A numeric object of the the odds for each probability.

## See also

Other odds:
[`inv_odds()`](https://poissonconsulting.github.io/extras/dev/reference/inv_odds.md),
[`log_odds()`](https://poissonconsulting.github.io/extras/dev/reference/log_odds.md),
`log_odds<-()`,
[`log_odds_ratio()`](https://poissonconsulting.github.io/extras/dev/reference/log_odds_ratio.md),
`odds<-()`,
[`odds_ratio()`](https://poissonconsulting.github.io/extras/dev/reference/odds_ratio.md)

## Examples

``` r
odds(c(0, 0.5, 0.9, 1))
#> [1]   0   1   9 Inf
```
