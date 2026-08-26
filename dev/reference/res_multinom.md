# Multinomial Residuals

Models the counts across two or more mutually exclusive categories from
a fixed number of trials, in *long* format: one row per category per
trial, with `group` identifying which rows belong to the same trial (see
[`log_lik_multinom()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_multinom.md)
for details). `res_multinom()` returns one residual per row, not one per
trial, since a trial's categories aren't independent and so have no
single meaningful residual as a whole; the classic per-trial deviance
statistic can be recovered by summing the squared `type = "dev"`
residuals within a `group`.

## Usage

``` r
res_multinom(x, size = 1, prob, group, type = "dev", simulate = FALSE)
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

- type:

  A string of the residual type. 'raw' for raw residuals 'dev' for
  deviance residuals and 'data' for the data.

- simulate:

  A flag specifying whether to simulate residuals.

## Value

An numeric vector of the corresponding residuals.

## Details

`group` is validated (same `size`, `prob` summing to 1, no singleton or
short groups, no `NA`) regardless of `simulate`, but is only otherwise
used when `simulate = TRUE`, to draw a joint, correlation-preserving
replicate per trial (via
[`ran_multinom()`](https://poissonconsulting.github.io/extras/dev/reference/ran_multinom.md))
rather than simulating each category independently, which requires
`res_multinom()` to see every row of a `group` in the same call.

## References

Haberman, S.J. 1973. The analysis of residuals in cross-classified
tables. Biometrics 29(1): 205-220.
[doi:10.2307/2529686](https://doi.org/10.2307/2529686) .

Pierce, D.A., and Schafer, D.W. 1986. Residuals in generalized linear
models. Journal of the American Statistical Association 81(396):
977-986.
[doi:10.1080/01621459.1986.10478361](https://doi.org/10.1080/01621459.1986.10478361)
.

Gelman, A., Meng, X.-L., and Stern, H. 1996. Posterior predictive
assessment of model fitness via realized discrepancies. Statistica
Sinica 6(4): 733-807.

## See also

Other res_dist:
[`res_bern()`](https://poissonconsulting.github.io/extras/dev/reference/res_bern.md),
[`res_beta_binom()`](https://poissonconsulting.github.io/extras/dev/reference/res_beta_binom.md),
[`res_binom()`](https://poissonconsulting.github.io/extras/dev/reference/res_binom.md),
[`res_gamma()`](https://poissonconsulting.github.io/extras/dev/reference/res_gamma.md),
[`res_gamma_pois()`](https://poissonconsulting.github.io/extras/dev/reference/res_gamma_pois.md),
[`res_gamma_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/res_gamma_pois_zi.md),
[`res_lnorm()`](https://poissonconsulting.github.io/extras/dev/reference/res_lnorm.md),
[`res_neg_binom()`](https://poissonconsulting.github.io/extras/dev/reference/res_neg_binom.md),
[`res_norm()`](https://poissonconsulting.github.io/extras/dev/reference/res_norm.md),
[`res_pois()`](https://poissonconsulting.github.io/extras/dev/reference/res_pois.md),
[`res_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/res_pois_zi.md),
[`res_skewlnorm()`](https://poissonconsulting.github.io/extras/dev/reference/res_skewlnorm.md),
[`res_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/res_skewnorm.md),
[`res_student()`](https://poissonconsulting.github.io/extras/dev/reference/res_student.md)

## Examples

``` r
res_multinom(c(1, 3, 6), size = 10, prob = c(0.2, 0.3, 0.5), group = c(1, 1, 1))
#> [1] -0.7833937  0.0000000  0.4334267
```
