# Bernoulli Residuals

Bernoulli Residuals

## Usage

``` r
res_bern(x, prob = 0.5, type = "dev", simulate = FALSE)
```

## Arguments

- x:

  A vector of 0s and 1s.

- prob:

  A numeric vector of values between 0 and 1 of the probability of
  success.

- type:

  A string of the residual type. 'raw' for raw residuals 'dev' for
  deviance residuals and 'data' for the data.

- simulate:

  A flag specifying whether to simulate residuals.

## Value

An numeric vector of the corresponding residuals.

## See also

Other res_dist:
[`res_beta_binom()`](https://poissonconsulting.github.io/extras/dev/reference/res_beta_binom.md),
[`res_binom()`](https://poissonconsulting.github.io/extras/dev/reference/res_binom.md),
[`res_gamma()`](https://poissonconsulting.github.io/extras/dev/reference/res_gamma.md),
[`res_gamma_pois()`](https://poissonconsulting.github.io/extras/dev/reference/res_gamma_pois.md),
[`res_gamma_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/res_gamma_pois_zi.md),
[`res_lnorm()`](https://poissonconsulting.github.io/extras/dev/reference/res_lnorm.md),
[`res_neg_binom()`](https://poissonconsulting.github.io/extras/dev/reference/res_neg_binom.md),
[`res_norm()`](https://poissonconsulting.github.io/extras/dev/reference/res_norm.md),
[`res_pois()`](https://poissonconsulting.github.io/extras/dev/reference/res_pois.md),
[`res_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/res_pois_zi.md),
[`res_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/res_skewnorm.md),
[`res_student()`](https://poissonconsulting.github.io/extras/dev/reference/res_student.md)

## Examples

``` r
res_bern(c(TRUE, FALSE), 0.7)
#> [1]  0.8446004 -1.5517557
```
