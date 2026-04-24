# Negative Binomial Deviances

Negative Binomial Deviances

## Usage

``` r
dev_neg_binom(x, lambda = 1, theta = 0, res = FALSE)
```

## Arguments

- x:

  A non-negative whole numeric vector of values.

- lambda:

  A non-negative numeric vector of means.

- theta:

  A non-negative numeric vector of the dispersion for the mixture models
  (student, gamma-Poisson and beta-binomial).

- res:

  A flag specifying whether to return the deviance residual as opposed
  to the deviance.

## Value

An numeric vector of the corresponding deviances or deviance residuals.

## See also

Other dev_dist:
[`dev_bern()`](https://poissonconsulting.github.io/extras/dev/reference/dev_bern.md),
[`dev_beta_binom()`](https://poissonconsulting.github.io/extras/dev/reference/dev_beta_binom.md),
[`dev_binom()`](https://poissonconsulting.github.io/extras/dev/reference/dev_binom.md),
[`dev_gamma()`](https://poissonconsulting.github.io/extras/dev/reference/dev_gamma.md),
[`dev_gamma_pois()`](https://poissonconsulting.github.io/extras/dev/reference/dev_gamma_pois.md),
[`dev_lnorm()`](https://poissonconsulting.github.io/extras/dev/reference/dev_lnorm.md),
[`dev_norm()`](https://poissonconsulting.github.io/extras/dev/reference/dev_norm.md),
[`dev_pois()`](https://poissonconsulting.github.io/extras/dev/reference/dev_pois.md),
[`dev_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/dev_pois_zi.md),
[`dev_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/dev_skewnorm.md),
[`dev_student()`](https://poissonconsulting.github.io/extras/dev/reference/dev_student.md)

## Examples

``` r
dev_neg_binom(c(1, 2, 5), 2, 3)
#> [1] 0.1060144 0.0000000 0.3450025
```
