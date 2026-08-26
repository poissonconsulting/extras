# Multinomial Log-Likelihood

Models the counts across two or more mutually exclusive categories from
a fixed number of trials, in *long* format: one row per category per
trial, with `group` identifying which rows belong to the same trial. All
rows sharing a `group` must have the same `size`, and their `prob`
values must sum to 1.

## Usage

``` r
log_lik_multinom(x, size = 1, prob, group)
```

## Arguments

- x:

  A non-negative whole numeric vector of the category counts.

- size:

  A non-negative whole numeric vector of the number of trials.

- prob:

  A numeric vector of the probability of the category. Must sum to 1
  across the rows sharing the same `group`. `NA` in `size` or `prob` for
  any row of a trial makes the log-likelihood `NA` for every row of that
  trial, since a trial's categories are scored jointly.

- group:

  A vector identifying which rows belong to the same multinomial trial
  (whose `x` values sum to `size` and `prob` values sum to 1). Every
  group must have at least 2 rows and the same number of rows as the
  rest of the data (a fixed set of categories, as in multinomial
  logistic regression), and must not contain `NA`.

## Value

An numeric vector of the corresponding log-likelihoods, one value per
row of `x`.

## Details

A trial's log-likelihood doesn't split evenly across its rows, since the
multinomial coefficient belongs to the whole trial. `log_lik_multinom()`
uses the multinomial-as-independent-Poissons identity: each row's value
is the Poisson log-likelihood of `x` given `mu = size * prob`, minus an
even share of the trial's normalizing constant, so summing over a
`group` recovers the trial's exact multinomial log-likelihood.

## See also

Other log_lik_dist:
[`log_lik_bern()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_bern.md),
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
[`log_lik_skewlnorm()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_skewlnorm.md),
[`log_lik_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_skewnorm.md),
[`log_lik_student()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_student.md),
[`log_lik_unif()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_unif.md)

## Examples

``` r
log_lik_multinom(c(1, 3, 6), size = 10, prob = c(0.2, 0.3, 0.5), group = c(1, 1, 1))
#> [1] -0.6139989 -0.8030687 -1.2297699
```
