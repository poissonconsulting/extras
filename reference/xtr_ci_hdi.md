# Highest Density Interval

Calculates Bayesian credible intervals using the highest density
interval (HDI), i.e., the narrowest CI with the specified minimum
coverage.

## Usage

``` r
xtr_ci_hdi(x, level = 0.95, ..., na_rm = FALSE)
```

## Arguments

- x:

  A numeric vector of MCMC samples.

- level:

  A number \> 0 and \<= 1 specifying the probability coverage of the
  interval.

- ...:

  Currently unused.

- na_rm:

  A flag indicating whether to remove missing values.

## Value

A [data.frame](https://rdrr.io/r/base/data.frame.html) of the `lower`
and `upper` limits for the credible interval. Note that the interval is
not guaranteed to be one-sided or two-sided. Returns integer limits if
the input data are integers and double otherwise.

## See also

[`xtr_ci()`](https://poissonconsulting.github.io/extras/reference/xtr_ci.md)
and
[`xtr_ci_eti()`](https://poissonconsulting.github.io/extras/reference/xtr_ci_eti.md)

## Examples

``` r
xtr_ci_hdi(1:10, level = 0.1) # only 10% of values inside
#>   lower upper
#> 1     5     5
xtr_ci_hdi(1:10, level = 0.2) # only 20% of values inside
#>   lower upper
#> 1     5     6
xtr_ci_hdi(1:10, level = 0.2 + 0.01) # at least 20.1% of values inside
#>   lower upper
#> 1     4     6
xtr_ci_hdi(1:100) # inclusive interval [3, 98] with 95% of values inside
#>   lower upper
#> 1     3    97
```
