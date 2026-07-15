# Beta Cumulative Distribution Function

Beta Cumulative Distribution Function

## Usage

``` r
prob_beta(x, alpha = 1, beta = 1)
```

## Arguments

- x:

  A numeric vector of quantiles.

- alpha:

  The first shape parameter of the beta distribution.

- beta:

  The second shape parameter of the beta distribution.

## Value

An numeric vector of the corresponding probabilities.

## See also

Other prob_dist:
[`prob_bern()`](https://poissonconsulting.github.io/extras/reference/prob_bern.md),
[`prob_beta_binom()`](https://poissonconsulting.github.io/extras/reference/prob_beta_binom.md),
[`prob_binom()`](https://poissonconsulting.github.io/extras/reference/prob_binom.md),
[`prob_exp()`](https://poissonconsulting.github.io/extras/reference/prob_exp.md),
[`prob_gamma()`](https://poissonconsulting.github.io/extras/reference/prob_gamma.md),
[`prob_gamma_pois()`](https://poissonconsulting.github.io/extras/reference/prob_gamma_pois.md),
[`prob_gamma_pois_zi()`](https://poissonconsulting.github.io/extras/reference/prob_gamma_pois_zi.md),
[`prob_lnorm()`](https://poissonconsulting.github.io/extras/reference/prob_lnorm.md),
[`prob_neg_binom()`](https://poissonconsulting.github.io/extras/reference/prob_neg_binom.md),
[`prob_norm()`](https://poissonconsulting.github.io/extras/reference/prob_norm.md),
[`prob_pois()`](https://poissonconsulting.github.io/extras/reference/prob_pois.md),
[`prob_pois_zi()`](https://poissonconsulting.github.io/extras/reference/prob_pois_zi.md),
[`prob_skewlnorm()`](https://poissonconsulting.github.io/extras/reference/prob_skewlnorm.md),
[`prob_skewnorm()`](https://poissonconsulting.github.io/extras/reference/prob_skewnorm.md),
[`prob_student()`](https://poissonconsulting.github.io/extras/reference/prob_student.md),
[`prob_unif()`](https://poissonconsulting.github.io/extras/reference/prob_unif.md)

## Examples

``` r
prob_beta(c(0, 0.5, 1), 2, 3)
#> [1] 0.0000 0.6875 1.0000
```
