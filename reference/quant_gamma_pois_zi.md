# Zero-Inflated Gamma-Poisson Quantile Function

Zero-Inflated Gamma-Poisson Quantile Function

## Usage

``` r
quant_gamma_pois_zi(x, lambda = 1, theta = 0, prob = 0)
```

## Arguments

- x:

  A numeric vector of probabilities.

- lambda:

  A non-negative numeric vector of means.

- theta:

  A non-negative numeric vector of the dispersion for the mixture models
  (student, gamma-Poisson and beta-binomial).

- prob:

  A numeric vector of values between 0 and 1 of the probability of
  success.

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
[`quant_lnorm()`](https://poissonconsulting.github.io/extras/reference/quant_lnorm.md),
[`quant_neg_binom()`](https://poissonconsulting.github.io/extras/reference/quant_neg_binom.md),
[`quant_norm()`](https://poissonconsulting.github.io/extras/reference/quant_norm.md),
[`quant_pois()`](https://poissonconsulting.github.io/extras/reference/quant_pois.md),
[`quant_pois_zi()`](https://poissonconsulting.github.io/extras/reference/quant_pois_zi.md),
[`quant_skewlnorm()`](https://poissonconsulting.github.io/extras/reference/quant_skewlnorm.md),
[`quant_skewnorm()`](https://poissonconsulting.github.io/extras/reference/quant_skewnorm.md),
[`quant_student()`](https://poissonconsulting.github.io/extras/reference/quant_student.md),
[`quant_unif()`](https://poissonconsulting.github.io/extras/reference/quant_unif.md)

## Examples

``` r
quant_gamma_pois_zi(c(0.1, 0.4, 0.6), 3, 1, prob = 0.5)
#> [1] 0 0 0
```
