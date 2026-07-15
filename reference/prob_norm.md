# Normal Cumulative Distribution Function

Normal Cumulative Distribution Function

## Usage

``` r
prob_norm(x, mean = 0, sd = 1)
```

## Arguments

- x:

  A numeric vector of quantiles.

- mean:

  A numeric vector of the means.

- sd:

  A non-negative numeric vector of the standard deviations.

## Value

An numeric vector of the corresponding probabilities.

## See also

Other prob_dist:
[`prob_bern()`](https://poissonconsulting.github.io/extras/reference/prob_bern.md),
[`prob_beta()`](https://poissonconsulting.github.io/extras/reference/prob_beta.md),
[`prob_beta_binom()`](https://poissonconsulting.github.io/extras/reference/prob_beta_binom.md),
[`prob_binom()`](https://poissonconsulting.github.io/extras/reference/prob_binom.md),
[`prob_exp()`](https://poissonconsulting.github.io/extras/reference/prob_exp.md),
[`prob_gamma()`](https://poissonconsulting.github.io/extras/reference/prob_gamma.md),
[`prob_gamma_pois()`](https://poissonconsulting.github.io/extras/reference/prob_gamma_pois.md),
[`prob_gamma_pois_zi()`](https://poissonconsulting.github.io/extras/reference/prob_gamma_pois_zi.md),
[`prob_lnorm()`](https://poissonconsulting.github.io/extras/reference/prob_lnorm.md),
[`prob_neg_binom()`](https://poissonconsulting.github.io/extras/reference/prob_neg_binom.md),
[`prob_pois()`](https://poissonconsulting.github.io/extras/reference/prob_pois.md),
[`prob_pois_zi()`](https://poissonconsulting.github.io/extras/reference/prob_pois_zi.md),
[`prob_skewlnorm()`](https://poissonconsulting.github.io/extras/reference/prob_skewlnorm.md),
[`prob_skewnorm()`](https://poissonconsulting.github.io/extras/reference/prob_skewnorm.md),
[`prob_student()`](https://poissonconsulting.github.io/extras/reference/prob_student.md),
[`prob_unif()`](https://poissonconsulting.github.io/extras/reference/prob_unif.md)

## Examples

``` r
prob_norm(c(-2:2))
#> [1] 0.02275013 0.15865525 0.50000000 0.84134475 0.97724987
```
