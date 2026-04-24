# Skew Normal Deviances

Skew Normal Deviances

## Usage

``` r
dev_skewnorm(x, mean = 0, sd = 1, shape = 0, res = FALSE)
```

## Arguments

- x:

  A numeric vector of values.

- mean:

  A numeric vector of the means.

- sd:

  A non-negative numeric vector of the standard deviations.

- shape:

  A numeric vector of shape.

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
[`dev_neg_binom()`](https://poissonconsulting.github.io/extras/dev/reference/dev_neg_binom.md),
[`dev_norm()`](https://poissonconsulting.github.io/extras/dev/reference/dev_norm.md),
[`dev_pois()`](https://poissonconsulting.github.io/extras/dev/reference/dev_pois.md),
[`dev_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/dev_pois_zi.md),
[`dev_student()`](https://poissonconsulting.github.io/extras/dev/reference/dev_student.md)

## Examples

``` r
dev_skewnorm(c(-2:2))
#> [1] 4 1 0 1 4
dev_skewnorm(-2:2, 0, 1, 5)
#> [1] 110.2602054  30.9276319   1.1839294   0.7976357   3.7976351
dev_skewnorm(-2:2, 0, 1, 5, res = TRUE)
#> [1] -10.5004860  -5.5612617  -1.0880852   0.8931045   1.9487522
```
