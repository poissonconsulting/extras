# Gamma Deviances

Gamma Deviances

## Usage

``` r
dev_gamma(x, shape = 1, rate = 1, res = FALSE)
```

## Arguments

- x:

  A numeric vector of values.

- shape:

  A non-negative numeric vector of shape.

- rate:

  A non-negative numeric vector of rate.

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
dev_gamma(c(0, 1, 2), 1, 2)
#> [1]       Inf 0.6137056 3.2274113
```
