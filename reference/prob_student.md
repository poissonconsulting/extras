# Student's t Cumulative Distribution Function

Student's t Cumulative Distribution Function

## Usage

``` r
prob_student(x, mean = 0, sd = 1, theta = 0)
```

## Arguments

- x:

  A numeric vector of quantiles.

- mean:

  A numeric vector of the means.

- sd:

  A non-negative numeric vector of the standard deviations.

- theta:

  A non-negative numeric vector of the dispersion for the mixture models
  (student, gamma-Poisson and beta-binomial).

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
[`prob_norm()`](https://poissonconsulting.github.io/extras/reference/prob_norm.md),
[`prob_pois()`](https://poissonconsulting.github.io/extras/reference/prob_pois.md),
[`prob_pois_zi()`](https://poissonconsulting.github.io/extras/reference/prob_pois_zi.md),
[`prob_skewlnorm()`](https://poissonconsulting.github.io/extras/reference/prob_skewlnorm.md),
[`prob_skewnorm()`](https://poissonconsulting.github.io/extras/reference/prob_skewnorm.md),
[`prob_unif()`](https://poissonconsulting.github.io/extras/reference/prob_unif.md)

## Examples

``` r
prob_student(c(1, 3.5, 4), mean = 1, sd = 2, theta = 1 / 3)
#> [1] 0.5000000 0.8500353 0.8847081
```
