# Probability of Direction

The probability of direction (PD) is the proportion of the (posterior)
distribution above (right) or below (left) a threshold.

## Usage

``` r
probability_direction(x, side = "median", threshold = 0, na_rm = FALSE)
```

## Arguments

- x:

  A numeric vector of MCMC values.

- side:

  A character vector of length 1 indicating whether to calculate the
  directional probability for the left tail (`"left"`; `x < threshold`),
  or the right tail (`"right"`; `x > threshold`). Defaults to
  `"median"`, which uses the side of the median of `x` via
  [`direction()`](https://poissonconsulting.github.io/extras/reference/direction.md).

- threshold:

  A number of the threshold value, which is excluded from the interval
  for the probability.

- na_rm:

  A flag specifying whether to remove missing values.

## Value

A number between 0 and 1. If `x` has `NA` values but `na_rm` is `FALSE`,
returns `NA_real`.

## Details

By default, the direction is based on the side of the median value, but
it can be specified to measure support for specific hypotheses. A
right-side PD of 0.9 indicates that the interval spanning from the
threshold to infinity has a coverage of 90%. Can be used as a measure of
certainty in the direction of the estimate (e.g., positive or negative
when using a threshold of 0). **NOTE:** probability estimates of 0 or 1
are corrected towards 0.5 by adding or subtracting
`1 / (length(x) + 1)`, where `x` is a vector of MCMC samples. Ideally,
`x` should be large enough as to make the correction negligible.

## References

Makowski, D., Ben-Shachar, M.S., Chen, S.H.A., and Lüdecke, D. 2019.
Indices of Effect Existence and Significance in the Bayesian Framework.
Front. Psychol. 10: 2767.
[doi:10.3389/fpsyg.2019.02767](https://doi.org/10.3389/fpsyg.2019.02767)
.

## See also

Other summary:
[`direction()`](https://poissonconsulting.github.io/extras/reference/direction.md),
[`directional_information()`](https://poissonconsulting.github.io/extras/reference/directional-information.md),
[`kurtosis()`](https://poissonconsulting.github.io/extras/reference/kurtosis.md),
[`lower()`](https://poissonconsulting.github.io/extras/reference/lower.md),
[`pvalue()`](https://poissonconsulting.github.io/extras/reference/pvalue.md),
[`pzeros()`](https://poissonconsulting.github.io/extras/reference/pzeros.md),
[`skewness()`](https://poissonconsulting.github.io/extras/reference/skewness.md),
[`svalue()`](https://poissonconsulting.github.io/extras/reference/svalue.md),
[`upper()`](https://poissonconsulting.github.io/extras/reference/upper.md),
[`variance()`](https://poissonconsulting.github.io/extras/reference/variance.md),
[`xtr_mean()`](https://poissonconsulting.github.io/extras/reference/xtr_mean.md),
[`xtr_median()`](https://poissonconsulting.github.io/extras/reference/xtr_median.md),
[`xtr_sd()`](https://poissonconsulting.github.io/extras/reference/xtr_sd.md),
[`zeros()`](https://poissonconsulting.github.io/extras/reference/zeros.md),
[`zscore()`](https://poissonconsulting.github.io/extras/reference/zscore.md)

## Examples

``` r
x <- rnorm(1e6, qnorm(0.05, lower.tail = TRUE))
probability_direction(x, side = "left")
#> [1] 0.949519
probability_direction(x, side = "right") # = 1 - probability_direction(x, side = "left")
#> [1] 0.050481
probability_direction(c(0, 0, 1), side = "right") # returns P(X >0) = 1/3 instead of P(X >= 0) = 1
#> [1] 0.3333333
probability_direction(c(1, 1), side = "right") # p = 1 - 1/(n+1)
#> [1] 0.6666667
```
