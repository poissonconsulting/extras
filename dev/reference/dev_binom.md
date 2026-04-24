# Binomial Deviances

Binomial Deviances

## Usage

``` r
dev_binom(x, size = 1, prob = 0.5, res = FALSE)
```

## Arguments

- x:

  A non-negative whole numeric vector of values.

- size:

  A non-negative whole numeric vector of the number of trials.

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
[`dev_bern()`](https://poissonconsulting.github.io/extras/dev/reference/dev_bern.md),
[`dev_beta_binom()`](https://poissonconsulting.github.io/extras/dev/reference/dev_beta_binom.md),
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
dev_binom(c(0, 1, 2), 2, 0.3)
#> [1] 1.4266998 0.3487068 4.8158912
```
