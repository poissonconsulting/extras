# Skew-Lognormal Quantile Function

Skew-Lognormal Quantile Function

## Usage

``` r
quant_skewlnorm(
  x,
  locationlog = 0,
  scalelog = 1,
  shapelog = 0,
  ...,
  meanlog,
  sdlog,
  shape
)
```

## Arguments

- x:

  A numeric vector of probabilities.

- locationlog:

  A numeric vector of location parameters of `log(x)`.

- scalelog:

  A non-negative numeric vector of scale parameters of `log(x)`.

- shapelog:

  A numeric vector of shape parameters of `log(x)`. Negative values
  result in leftward skew, while positive values result in rightward
  skew.

- ...:

  Unused.

- meanlog:

  **\[deprecated\]** A numeric vector of location parameters of
  `log(x)`. Described as "a numeric vector of the means on the log
  scale" prior to v. 0.10.1. Will be removed in a future version.

- sdlog:

  **\[deprecated\]** A non-negative numeric vector of scale parameters
  of `log(x)`. Described as "a non-negative numeric vector of the
  standard deviations on the log scale" prior to v. 0.10.1. Will be
  removed in a future version.

- shape:

  **\[deprecated\]** A numeric vector of shape parameters of `log(x)`.
  Will be removed in a future version.

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
[`quant_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/quant_skewnorm.md),
[`quant_student()`](https://poissonconsulting.github.io/extras/dev/reference/quant_student.md),
[`quant_unif()`](https://poissonconsulting.github.io/extras/dev/reference/quant_unif.md)

## Examples

``` r
quant_skewlnorm(c(0.1, 0.4, 0.6))
#> [1] 0.2776062 0.7761984 1.2883304
quant_skewlnorm(c(0.1, 0.4, 0.6), shapelog = -2)
#> [1] 0.1930513 0.4346470 0.6150948
quant_skewlnorm(c(0.1, 0.4, 0.6), shapelog = 2)
#> [1] 0.8747552 1.6257658 2.3007178
```
