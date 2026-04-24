# Bernoulli Deviances

Bernoulli Deviances

## Usage

``` r
dev_bern(x, prob = 0.5, res = FALSE)
```

## Arguments

- x:

  A vector of 0s and 1s.

- prob:

  A numeric vector of values between 0 and 1 of the probability of
  success.

- res:

  A flag specifying whether to return the deviance residual as opposed
  to the deviance.

## Value

An numeric vector of the corresponding deviances or deviance residuals.

## See also

Other dev_dist:
[`dev_beta_binom()`](https://poissonconsulting.github.io/extras/dev/reference/dev_beta_binom.md),
[`dev_binom()`](https://poissonconsulting.github.io/extras/dev/reference/dev_binom.md),
[`dev_gamma()`](https://poissonconsulting.github.io/extras/dev/reference/dev_gamma.md),
[`dev_gamma_pois()`](https://poissonconsulting.github.io/extras/dev/reference/dev_gamma_pois.md),
[`dev_lnorm()`](https://poissonconsulting.github.io/extras/dev/reference/dev_lnorm.md),
[`dev_neg_binom()`](https://poissonconsulting.github.io/extras/dev/reference/dev_neg_binom.md),
[`dev_norm()`](https://poissonconsulting.github.io/extras/dev/reference/dev_norm.md),
[`dev_pois()`](https://poissonconsulting.github.io/extras/dev/reference/dev_pois.md),
[`dev_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/dev_pois_zi.md),
[`dev_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/dev_skewnorm.md),
[`dev_student()`](https://poissonconsulting.github.io/extras/dev/reference/dev_student.md)

## Examples

``` r
dev_bern(c(TRUE, FALSE), 0.7)
#> [1] 0.7133499 2.4079456
```
