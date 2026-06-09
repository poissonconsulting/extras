# Uniform Quantile Function

Uniform Quantile Function

## Usage

``` r
quant_unif(x, min = 0, max = 1)
```

## Arguments

- x:

  A numeric vector of probabilities.

- min:

  A numeric vector of the minimums.

- max:

  A numeric vector of the maximums.

## Value

An numeric vector of the corresponding quantiles.

## See also

Other quant_dist:
[`quant_bern()`](https://poissonconsulting.github.io/extras/reference/quant_bern.md),
[`quant_beta()`](https://poissonconsulting.github.io/extras/reference/quant_beta.md),
[`quant_binom()`](https://poissonconsulting.github.io/extras/reference/quant_binom.md),
[`quant_exp()`](https://poissonconsulting.github.io/extras/reference/quant_exp.md),
[`quant_gamma()`](https://poissonconsulting.github.io/extras/reference/quant_gamma.md),
[`quant_gamma_pois()`](https://poissonconsulting.github.io/extras/reference/quant_gamma_pois.md),
[`quant_gamma_pois_zi()`](https://poissonconsulting.github.io/extras/reference/quant_gamma_pois_zi.md),
[`quant_lnorm()`](https://poissonconsulting.github.io/extras/reference/quant_lnorm.md),
[`quant_neg_binom()`](https://poissonconsulting.github.io/extras/reference/quant_neg_binom.md),
[`quant_norm()`](https://poissonconsulting.github.io/extras/reference/quant_norm.md),
[`quant_pois()`](https://poissonconsulting.github.io/extras/reference/quant_pois.md),
[`quant_pois_zi()`](https://poissonconsulting.github.io/extras/reference/quant_pois_zi.md),
[`quant_skewnorm()`](https://poissonconsulting.github.io/extras/reference/quant_skewnorm.md),
[`quant_student()`](https://poissonconsulting.github.io/extras/reference/quant_student.md)

## Examples

``` r
quant_unif(c(0.1, 0.4, 0.6))
#> [1] 0.1 0.4 0.6
```
