# Exponential Cumulative Distribution Function

Exponential Cumulative Distribution Function

## Usage

``` r
prob_exp(x, rate = 1)
```

## Arguments

- x:

  A numeric vector of quantiles.

- rate:

  A non-negative numeric vector of rate.

## Value

An numeric vector of the corresponding probabilities.

## See also

Other prob_dist:
[`prob_bern()`](https://poissonconsulting.github.io/extras/dev/reference/prob_bern.md),
[`prob_beta()`](https://poissonconsulting.github.io/extras/dev/reference/prob_beta.md),
[`prob_beta_binom()`](https://poissonconsulting.github.io/extras/dev/reference/prob_beta_binom.md),
[`prob_binom()`](https://poissonconsulting.github.io/extras/dev/reference/prob_binom.md),
[`prob_gamma()`](https://poissonconsulting.github.io/extras/dev/reference/prob_gamma.md),
[`prob_gamma_pois()`](https://poissonconsulting.github.io/extras/dev/reference/prob_gamma_pois.md),
[`prob_gamma_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/prob_gamma_pois_zi.md),
[`prob_lnorm()`](https://poissonconsulting.github.io/extras/dev/reference/prob_lnorm.md),
[`prob_neg_binom()`](https://poissonconsulting.github.io/extras/dev/reference/prob_neg_binom.md),
[`prob_norm()`](https://poissonconsulting.github.io/extras/dev/reference/prob_norm.md),
[`prob_pois()`](https://poissonconsulting.github.io/extras/dev/reference/prob_pois.md),
[`prob_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/prob_pois_zi.md),
[`prob_skewlnorm()`](https://poissonconsulting.github.io/extras/dev/reference/prob_skewlnorm.md),
[`prob_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/prob_skewnorm.md),
[`prob_student()`](https://poissonconsulting.github.io/extras/dev/reference/prob_student.md),
[`prob_unif()`](https://poissonconsulting.github.io/extras/dev/reference/prob_unif.md)

## Examples

``` r
prob_exp(c(0, 1, 2), 2)
#> [1] 0.0000000 0.8646647 0.9816844
```
