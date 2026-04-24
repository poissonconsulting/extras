# Bernoulli Log-Likelihood

Bernoulli Log-Likelihood

## Usage

``` r
log_lik_bern(x, prob = 0.5)
```

## Arguments

- x:

  A vector of 0s and 1s.

- prob:

  A numeric vector of values between 0 and 1 of the probability of
  success.

## Value

An numeric vector of the corresponding log-likelihoods.

## See also

Other log_lik_dist:
[`log_lik_beta()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_beta.md),
[`log_lik_beta_binom()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_beta_binom.md),
[`log_lik_binom()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_binom.md),
[`log_lik_exp()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_exp.md),
[`log_lik_gamma()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_gamma.md),
[`log_lik_gamma_pois()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_gamma_pois.md),
[`log_lik_gamma_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_gamma_pois_zi.md),
[`log_lik_lnorm()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_lnorm.md),
[`log_lik_neg_binom()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_neg_binom.md),
[`log_lik_norm()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_norm.md),
[`log_lik_pois()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_pois.md),
[`log_lik_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_pois_zi.md),
[`log_lik_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_skewnorm.md),
[`log_lik_student()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_student.md),
[`log_lik_unif()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_unif.md)

## Examples

``` r
log_lik_bern(c(TRUE, FALSE), 0.7)
#> [1] -0.3566749 -1.2039728
```
