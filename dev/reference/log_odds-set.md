# Inverse Log Odds Transformation

Replaces an object with the inverse log odds of value.

## Usage

``` r
log_odds(x) <- value
```

## Arguments

- x:

  An existing R object.

- value:

  A numeric atomic object.

## Value

Called for the side effect of updating `x`.

## See also

Other odds:
[`inv_odds()`](https://poissonconsulting.github.io/extras/dev/reference/inv_odds.md),
[`log_odds()`](https://poissonconsulting.github.io/extras/dev/reference/log_odds.md),
[`log_odds_ratio()`](https://poissonconsulting.github.io/extras/dev/reference/log_odds_ratio.md),
[`odds()`](https://poissonconsulting.github.io/extras/dev/reference/odds.md),
`odds<-()`,
[`odds_ratio()`](https://poissonconsulting.github.io/extras/dev/reference/odds_ratio.md)

## Examples

``` r
x <- NULL
log_odds(x) <- 0.5
x
#> [1] -1.098612
```
