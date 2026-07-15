# Credible Intervals

Calculates Bayesian credible intervals (CI) using one of the available
methods:

- `"HDI"`: highest density interval (see
  [`xtr_ci_hdi()`](https://poissonconsulting.github.io/extras/reference/xtr_ci_hdi.md)),

- `"ETI"`: equal tailed intervals (see
  [`xtr_ci_eti()`](https://poissonconsulting.github.io/extras/reference/xtr_ci_eti.md)).

## Usage

``` r
xtr_ci(x, level = 0.95, ..., type = "HDI", na_rm = FALSE)
```

## Arguments

- x:

  A numeric vector of MCMC samples.

- level:

  A number \> 0 and \<= 1 specifying the probability coverage of the
  interval.

- ...:

  Currently unused.

- type:

  A string indicating which type of CI to return. Currently allows
  Highest Density Intervals (`"HDI"`; default) and Equal-Tailed
  Intervals (`"ETI"`).

- na_rm:

  A flag indicating whether to remove missing values.

## Value

A [data.frame](https://rdrr.io/r/base/data.frame.html) of the `lower`
and `upper` limits for the credible interval. Note that the interval is
not guaranteed to be one-sided or two-sided.

## See also

mcmcr::coef

## Examples

``` r
xtr_ci(rnorm(1e4), type = "HDI")
#>       lower    upper
#> 1 -1.943616 1.960297
```
