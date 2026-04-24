# Inverse Odds

Calculates the probabilities for odds.

## Usage

``` r
inv_odds(x)
```

## Arguments

- x:

  A numeric object (vector, matrix or array) of odds.

## Value

A numeric object of the the probabilities for each odd.

## See also

Other odds:
[`log_odds()`](https://poissonconsulting.github.io/extras/dev/reference/log_odds.md),
`log_odds<-()`,
[`log_odds_ratio()`](https://poissonconsulting.github.io/extras/dev/reference/log_odds_ratio.md),
[`odds()`](https://poissonconsulting.github.io/extras/dev/reference/odds.md),
`odds<-()`,
[`odds_ratio()`](https://poissonconsulting.github.io/extras/dev/reference/odds_ratio.md)

## Examples

``` r
inv_odds(c(0, 1, 9, 9999))
#> [1] 0.0000 0.5000 0.9000 0.9999
```
