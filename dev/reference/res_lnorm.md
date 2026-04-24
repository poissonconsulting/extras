# Log-Normal Residuals

Log-Normal Residuals

## Usage

``` r
res_lnorm(x, meanlog = 0, sdlog = 1, type = "dev", simulate = FALSE)
```

## Arguments

- x:

  A numeric vector of values.

- meanlog:

  A numeric vector of the means on the log scale.

- sdlog:

  A non-negative numeric vector of the standard deviations on the log
  scale.

- type:

  A string of the residual type. 'raw' for raw residuals 'dev' for
  deviance residuals and 'data' for the data.

- simulate:

  A flag specifying whether to simulate residuals.

## Value

An numeric vector of the corresponding residuals.

## See also

Other res_dist:
[`res_bern()`](https://poissonconsulting.github.io/extras/dev/reference/res_bern.md),
[`res_beta_binom()`](https://poissonconsulting.github.io/extras/dev/reference/res_beta_binom.md),
[`res_binom()`](https://poissonconsulting.github.io/extras/dev/reference/res_binom.md),
[`res_gamma()`](https://poissonconsulting.github.io/extras/dev/reference/res_gamma.md),
[`res_gamma_pois()`](https://poissonconsulting.github.io/extras/dev/reference/res_gamma_pois.md),
[`res_gamma_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/res_gamma_pois_zi.md),
[`res_neg_binom()`](https://poissonconsulting.github.io/extras/dev/reference/res_neg_binom.md),
[`res_norm()`](https://poissonconsulting.github.io/extras/dev/reference/res_norm.md),
[`res_pois()`](https://poissonconsulting.github.io/extras/dev/reference/res_pois.md),
[`res_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/res_pois_zi.md),
[`res_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/res_skewnorm.md),
[`res_student()`](https://poissonconsulting.github.io/extras/dev/reference/res_student.md)

## Examples

``` r
res_lnorm(10)
#> [1] 2.302585
```
