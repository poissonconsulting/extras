# Skew Normal Quantile Function

Skew Normal Quantile Function

## Usage

``` r
quant_skewnorm(x, location = 0, scale = 1, shape = 0, ..., mean, sd)
```

## Arguments

- x:

  A numeric vector of probabilities.

- location:

  A numeric vector of the location parameter.

- scale:

  A non-negative numeric vector of the scale parameter.

- shape:

  A numeric vector of shape. Negative values result in leftward skew,
  while positive values result in rightward skew.

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

An numeric vector of the corresponding quantiles.

## See also

Other quant_dist:
[`quant_bern()`](https://poissonconsulting.github.io/extras/dev/reference/quant_bern.md),
[`quant_beta()`](https://poissonconsulting.github.io/extras/dev/reference/quant_beta.md),
[`quant_binom()`](https://poissonconsulting.github.io/extras/dev/reference/quant_binom.md),
[`quant_exp()`](https://poissonconsulting.github.io/extras/dev/reference/quant_exp.md),
[`quant_gamma()`](https://poissonconsulting.github.io/extras/dev/reference/quant_gamma.md),
[`quant_gamma_pois()`](https://poissonconsulting.github.io/extras/dev/reference/quant_gamma_pois.md),
[`quant_gamma_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/quant_gamma_pois_zi.md),
[`quant_lnorm()`](https://poissonconsulting.github.io/extras/dev/reference/quant_lnorm.md),
[`quant_neg_binom()`](https://poissonconsulting.github.io/extras/dev/reference/quant_neg_binom.md),
[`quant_norm()`](https://poissonconsulting.github.io/extras/dev/reference/quant_norm.md),
[`quant_pois()`](https://poissonconsulting.github.io/extras/dev/reference/quant_pois.md),
[`quant_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/quant_pois_zi.md),
[`quant_skewlnorm()`](https://poissonconsulting.github.io/extras/dev/reference/quant_skewlnorm.md),
[`quant_student()`](https://poissonconsulting.github.io/extras/dev/reference/quant_student.md),
[`quant_unif()`](https://poissonconsulting.github.io/extras/dev/reference/quant_unif.md)

## Examples

``` r
quant_skewnorm(c(0.1, 0.4, 0.6))
#> [1] -1.2815516 -0.2533471  0.2533471
quant_skewnorm(c(0.1, 0.4, 0.6), shape = -2)
#> [1] -1.6447994 -0.8332211 -0.4859789
quant_skewnorm(c(0.1, 0.4, 0.6), shape = 2)
#> [1] -0.1338113  0.4859789  0.8332211
```
