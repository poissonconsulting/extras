# Equal-Tailed Interval

Calculates Bayesian credible intervals using the equal-tailed interval
(ETI), i.e., the CI such that the left and right tails outside the CI
have the same coverage.

## Usage

``` r
xtr_ci_eti(x, level = 0.95, ..., na_rm = FALSE)
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
and `upper` limits for the credible interval.

## Details

The interval is guaranteed to be two-sided, unlike `[xtr_ci_hdi()]`.
Does not return integer outputs even if the input data are integers,
unlike
[`xtr_ci_hdi()`](https://poissonconsulting.github.io/extras/dev/reference/xtr_ci_hdi.md).
The interval limits are always real (double) numeric values.

## See also

[`xtr_ci()`](https://poissonconsulting.github.io/extras/dev/reference/xtr_ci.md)
and
[`xtr_ci_hdi()`](https://poissonconsulting.github.io/extras/dev/reference/xtr_ci_hdi.md)

## Examples

``` r
xtr_ci_eti(rnorm(1e4))
#>      lower    upper
#> 1 -1.94765 1.943883
```
