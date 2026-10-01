# Skew Normal Residuals

Skew Normal Residuals

## Usage

``` r
res_skewnorm(
  x,
  location = 0,
  scale = 1,
  shape = 0,
  type = "dev",
  simulate = FALSE,
  ...,
  mean,
  sd
)
```

## Arguments

- x:

  A numeric vector of values.

- location:

  A numeric vector of the location parameter.

- scale:

  A non-negative numeric vector of the scale parameter.

- shape:

  A numeric vector of shape. Negative values result in leftward skew,
  while positive values result in rightward skew.

- type:

  A string of the residual type. 'raw' for raw residuals 'dev' for
  deviance residuals and 'data' for the data.

- simulate:

  A flag specifying whether to simulate residuals.

- ...:

  Unused.

- mean:

  **\[deprecated\]** A numeric vector of the location parameter.
  Described as "a numeric vector of the means" prior to to v. 0.10.1.
  Will be removed in a future version.

- sd:

  **\[deprecated\]** A non-negative numeric vector of the scale
  parameter. Described as "a non-negative numeric vector of the standard
  deviations" prior to v. 0.10.1. Will be removed in a future version.

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
[`res_lnorm()`](https://poissonconsulting.github.io/extras/dev/reference/res_lnorm.md),
[`res_multinom()`](https://poissonconsulting.github.io/extras/dev/reference/res_multinom.md),
[`res_neg_binom()`](https://poissonconsulting.github.io/extras/dev/reference/res_neg_binom.md),
[`res_norm()`](https://poissonconsulting.github.io/extras/dev/reference/res_norm.md),
[`res_pois()`](https://poissonconsulting.github.io/extras/dev/reference/res_pois.md),
[`res_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/res_pois_zi.md),
[`res_skewlnorm()`](https://poissonconsulting.github.io/extras/dev/reference/res_skewlnorm.md),
[`res_student()`](https://poissonconsulting.github.io/extras/dev/reference/res_student.md)

## Examples

``` r
res_skewnorm(c(-2:2))
#> [1] -2 -1  0  1  2
```
