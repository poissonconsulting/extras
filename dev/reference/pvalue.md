# Bayesian P-Value

A Bayesian p-value (p) is here defined in terms of the quantile-based
(1-p) \* 100% credible interval (CRI) that just includes a threshold
(Kery and Schaub 2011). By default a p-value of 0.05 indicates that the
95% CRI just includes 0.

## Usage

``` r
pvalue(x, threshold = 0, side = "both", na_rm = FALSE)
```

## Arguments

- x:

  A numeric vector of MCMC values.

- threshold:

  A number of the threshold value.

- side:

  A character vector of length 1 indicating whether to calculate
  p-values for the left tail (`"left"`), right tail (`"right"`), or
  two-sided (`"both"`; default).

- na_rm:

  A flag specifying whether to remove missing values.

## Value

A number between 0 and 1.

## References

Kery, M., and Schaub, M. 2011. Bayesian population analysis using
WinBUGS: a hierarchical perspective. Academic Press, Boston. Available
from
<https://www.vogelwarte.ch/en/research/population-biology/book-bpa/>.

## See also

Other summary:
[`kurtosis()`](https://poissonconsulting.github.io/extras/dev/reference/kurtosis.md),
[`lower()`](https://poissonconsulting.github.io/extras/dev/reference/lower.md),
[`pzeros()`](https://poissonconsulting.github.io/extras/dev/reference/pzeros.md),
[`skewness()`](https://poissonconsulting.github.io/extras/dev/reference/skewness.md),
[`svalue()`](https://poissonconsulting.github.io/extras/dev/reference/svalue.md),
[`upper()`](https://poissonconsulting.github.io/extras/dev/reference/upper.md),
[`variance()`](https://poissonconsulting.github.io/extras/dev/reference/variance.md),
[`xtr_mean()`](https://poissonconsulting.github.io/extras/dev/reference/xtr_mean.md),
[`xtr_median()`](https://poissonconsulting.github.io/extras/dev/reference/xtr_median.md),
[`xtr_sd()`](https://poissonconsulting.github.io/extras/dev/reference/xtr_sd.md),
[`zeros()`](https://poissonconsulting.github.io/extras/dev/reference/zeros.md),
[`zscore()`](https://poissonconsulting.github.io/extras/dev/reference/zscore.md)

## Examples

``` r
x <- rnorm(1e6, qnorm(0.05, lower.tail = TRUE))
pvalue(x) # should be 0.05 * 2
#> [1] 0.0998769
pvalue(x, side = "left") # should be 0.95
#> [1] 0.950062
pvalue(x, side = "right") # should be 0.05
#> [1] 0.04993895
```
