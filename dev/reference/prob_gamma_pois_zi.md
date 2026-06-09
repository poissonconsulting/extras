# Zero-Inflated Gamma-Poisson Cumulative Distribution Function

Zero-Inflated Gamma-Poisson Cumulative Distribution Function

## Usage

``` r
prob_gamma_pois_zi(x, lambda = 1, theta = 0, prob = 0)
```

## Arguments

- x:

  A numeric vector of quantiles.

- lambda:

  A non-negative numeric vector of means.

- theta:

  A non-negative numeric vector of the dispersion for the mixture models
  (student, gamma-Poisson and beta-binomial).

- prob:

  A numeric vector of values between 0 and 1 of the probability of
  success.

## Value

An numeric vector of the corresponding probabilities.

## See also

Other prob_dist:
[`prob_bern()`](https://poissonconsulting.github.io/extras/dev/reference/prob_bern.md),
[`prob_beta()`](https://poissonconsulting.github.io/extras/dev/reference/prob_beta.md),
[`prob_beta_binom()`](https://poissonconsulting.github.io/extras/dev/reference/prob_beta_binom.md),
[`prob_binom()`](https://poissonconsulting.github.io/extras/dev/reference/prob_binom.md),
[`prob_exp()`](https://poissonconsulting.github.io/extras/dev/reference/prob_exp.md),
[`prob_gamma()`](https://poissonconsulting.github.io/extras/dev/reference/prob_gamma.md),
[`prob_gamma_pois()`](https://poissonconsulting.github.io/extras/dev/reference/prob_gamma_pois.md),
[`prob_lnorm()`](https://poissonconsulting.github.io/extras/dev/reference/prob_lnorm.md),
[`prob_neg_binom()`](https://poissonconsulting.github.io/extras/dev/reference/prob_neg_binom.md),
[`prob_norm()`](https://poissonconsulting.github.io/extras/dev/reference/prob_norm.md),
[`prob_pois()`](https://poissonconsulting.github.io/extras/dev/reference/prob_pois.md),
[`prob_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/prob_pois_zi.md),
[`prob_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/prob_skewnorm.md),
[`prob_student()`](https://poissonconsulting.github.io/extras/dev/reference/prob_student.md),
[`prob_unif()`](https://poissonconsulting.github.io/extras/dev/reference/prob_unif.md)

## Examples

``` r
prob_gamma_pois_zi(c(1, 3, 4), 3, 1, prob = 0.5)
#> [1] 0.7187500 0.8417969 0.8813477
```
