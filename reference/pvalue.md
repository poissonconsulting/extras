# Bayesian P-Value

A Bayesian p-value (p) is here defined in terms of the quantile-based
(1-p) \* 100% credible interval (CRI) that just includes a threshold
(Kery and Schaub 2011). By default a p-value of 0.05 indicates that the
95% CRI just includes the threshold value.Note that the function
contains the sample-size correction \\p\_{c} = p \* n / (n + 1)\\ to
avoid p-values of 0. The function can still return p-values of 1.To use
as a measure of certainty in the direction of the estimate (i.e.,
positive or negative), see
[`probability_direction()`](https://poissonconsulting.github.io/extras/reference/probability_direction.md).For
p-values converted to bits, see
[`svalue()`](https://poissonconsulting.github.io/extras/reference/svalue.md).To
convert MCMC objects to information, see
[`directional_information()`](https://poissonconsulting.github.io/extras/reference/directional_information.md).

## Usage

``` r
pvalue(x, side = "both", threshold = 0, na_rm = FALSE)
```

## Arguments

- x:

  A numeric vector of MCMC values.

- side:

  A character vector of length 1 indicating whether to calculate
  p-values for the left tail (`"left"`), right tail (`"right"`), or
  two-sided (`"both"`; default).

- threshold:

  A number of the threshold value.

- na_rm:

  A flag specifying whether to remove missing values.

## Value

A number between 0 and 1. If `x` has `NA` values but `na_rm` is `FALSE`,
returns `NA_real`.

## References

Kery, M., and Schaub, M. 2011. Bayesian population analysis using
WinBUGS: a hierarchical perspective. Academic Press, Boston. Available
from
<https://www.vogelwarte.ch/en/research/population-biology/book-bpa/>.

## See also

Other summary:
[`direction()`](https://poissonconsulting.github.io/extras/reference/direction.md),
[`directional_information()`](https://poissonconsulting.github.io/extras/reference/directional_information.md),
[`kurtosis()`](https://poissonconsulting.github.io/extras/reference/kurtosis.md),
[`lower()`](https://poissonconsulting.github.io/extras/reference/lower.md),
[`probability_direction()`](https://poissonconsulting.github.io/extras/reference/probability_direction.md),
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
pvalue(x) # should be 0.05 * 2
#> [1] 0.099858
pvalue(x, side = "left") # should be 0.95
#> [1] 0.950071
pvalue(x, side = "right") # should be 0.05
#> [1] 0.049929
```
