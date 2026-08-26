# Multinomial Random Samples

Models the counts across two or more mutually exclusive categories from
a fixed number of trials, in *long* format: one value per category per
trial, with `group` identifying which rows belong to the same trial. All
rows sharing a `group` must have the same `size`, and their `prob`
values must sum to 1.

## Usage

``` r
ran_multinom(size = 1, prob, group)
```

## Arguments

- size:

  A non-negative whole numeric vector of the number of trials.

- prob:

  A numeric vector of the probability of the category. Must sum to 1
  across the rows sharing the same `group`. `NA` in `size` or `prob` for
  any row of a trial makes the sample `NA` for every row of that trial,
  since a trial's categories are drawn jointly.

- group:

  A vector identifying which rows belong to the same multinomial trial
  (whose `x` values sum to `size` and `prob` values sum to 1). Every
  group must have at least 2 rows and the same number of rows as the
  rest of the data (a fixed set of categories, as in multinomial
  logistic regression), and must not contain `NA`.

## Value

An integer vector of the random samples, one per row of `prob`.

## Details

Unlike the other `ran_*()` functions, `ran_multinom()` has no `n`
argument: the number of samples is fully determined by `length(prob)`
(equivalently `length(group)`), since a trial's categories can't be
generated independently of one another.

## See also

Other ran_dist:
[`ran_bern()`](https://poissonconsulting.github.io/extras/dev/reference/ran_bern.md),
[`ran_beta_binom()`](https://poissonconsulting.github.io/extras/dev/reference/ran_beta_binom.md),
[`ran_binom()`](https://poissonconsulting.github.io/extras/dev/reference/ran_binom.md),
[`ran_gamma()`](https://poissonconsulting.github.io/extras/dev/reference/ran_gamma.md),
[`ran_gamma_pois()`](https://poissonconsulting.github.io/extras/dev/reference/ran_gamma_pois.md),
[`ran_gamma_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/ran_gamma_pois_zi.md),
[`ran_lnorm()`](https://poissonconsulting.github.io/extras/dev/reference/ran_lnorm.md),
[`ran_neg_binom()`](https://poissonconsulting.github.io/extras/dev/reference/ran_neg_binom.md),
[`ran_norm()`](https://poissonconsulting.github.io/extras/dev/reference/ran_norm.md),
[`ran_pois()`](https://poissonconsulting.github.io/extras/dev/reference/ran_pois.md),
[`ran_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/ran_pois_zi.md),
[`ran_skewlnorm()`](https://poissonconsulting.github.io/extras/dev/reference/ran_skewlnorm.md),
[`ran_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/ran_skewnorm.md),
[`ran_student()`](https://poissonconsulting.github.io/extras/dev/reference/ran_student.md)

## Examples

``` r
ran_multinom(size = 10, prob = c(0.2, 0.3, 0.5), group = c(1, 1, 1))
#> [1] 3 2 5
```
