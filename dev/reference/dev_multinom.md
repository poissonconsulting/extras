# Multinomial Deviances

Models the counts across two or more mutually exclusive categories from
a fixed number of trials, in *long* format: one row per category per
trial, with `group` identifying which rows belong to the same trial.

## Usage

``` r
dev_multinom(x, size = 1, prob, group, res = FALSE)
```

## Arguments

- x:

  A non-negative whole numeric vector of the category counts.

- size:

  A non-negative whole numeric vector of the number of trials.

- prob:

  A numeric vector of the probability of the category. Must sum to 1
  across the rows sharing the same `group`.

- group:

  A vector identifying which rows belong to the same multinomial trial
  (whose `x` values sum to `size` and `prob` values sum to 1). Every
  group must have at least 2 rows and the same number of rows as the
  rest of the data (a fixed set of categories, as in multinomial
  logistic regression), and must not contain `NA`.

- res:

  A flag specifying whether to return the deviance residual as opposed
  to the deviance.

## Value

An numeric vector of the corresponding deviances or deviance residuals.

## Details

A category's deviance depends only on its own `x` and
`mu = size * prob`, not on the rest of its trial, so `group` is used
only to validate `size` and `prob` (see
[`log_lik_multinom()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_multinom.md)),
not in the calculation itself. `dev_multinom()` is the
Poisson-equivalent deviance (see
[`dev_pois()`](https://poissonconsulting.github.io/extras/dev/reference/dev_pois.md)):
summing it over a trial's rows recovers the trial's exact multinomial
deviance.

## References

McCullagh, P., and Nelder, J.A. 1989. Generalized Linear Models. 2nd
edition. Chapman and Hall, London.

Baker, S.G. 1994. The multinomial-Poisson transformation. The
Statistician 43(4): 495-504.
[doi:10.2307/2348134](https://doi.org/10.2307/2348134) .

Agresti, A. 2013. Categorical Data Analysis. 3rd edition. John Wiley and
Sons, Hoboken, New Jersey.

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
[`dev_skewlnorm()`](https://poissonconsulting.github.io/extras/dev/reference/dev_skewlnorm.md),
[`dev_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/dev_skewnorm.md),
[`dev_student()`](https://poissonconsulting.github.io/extras/dev/reference/dev_student.md)

## Examples

``` r
dev_multinom(c(1, 3, 6), size = 10, prob = c(0.2, 0.3, 0.5), group = c(1, 1, 1))
#> [1] 0.6137056 0.0000000 0.1878587
```
