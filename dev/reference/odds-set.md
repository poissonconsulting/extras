# Inverse Odds Transformation

Replaces an object with the inverse odds of value.

## Usage

``` r
odds(x) <- value
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
`log_odds<-()`,
[`log_odds_ratio()`](https://poissonconsulting.github.io/extras/dev/reference/log_odds_ratio.md),
[`odds()`](https://poissonconsulting.github.io/extras/dev/reference/odds.md),
[`odds_ratio()`](https://poissonconsulting.github.io/extras/dev/reference/odds_ratio.md)

## Examples

``` r
x <- NULL
odds(x) <- 0.5
x
#> [1] 0.3333333
```
