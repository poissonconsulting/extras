# Surprisal Value

The [surprisal
value](https://www.poissonconsulting.ca/post/2026/what-are-s-values/)
(Greenland 2019) is a probability expressed in terms of how many
consecutive heads would have to be thrown on a fair coin in a single
attempt to achieve the same probability: \\-\log_2(p)\\, where \\p\\ is
the p-value of interest. See the details section for some examples.

## Usage

``` r
svalue(x, ..., side = "both", threshold = 0, skeptical = TRUE, na_rm = FALSE)

p2svalue(p)
```

## Arguments

- x:

  A numeric object of MCMC values.

- ...:

  Unused.

- side:

  A character indicating whether to calculate s-values using p-values
  for the left tail (`"left"`), right tail (`"right"`), or both tails
  (`"both"`; default).

- threshold:

  A number of the threshold value.

- skeptical:

  A flag specifying whether or not to add one sample to the empty side
  of the threshold when 100% of samples are on one side. Avoids zero
  p-values and infinite s-values, and also imposes stronger bounds on
  directional information than \[-n, n\], which assume the MCMC samples
  are independent and representative.

- na_rm:

  A flag specifying whether to remove missing values.

- p:

  A numeric vector of probabilities.

## Value

A non-negative number. If `x` has `NA` values but `na_rm` is `FALSE`,
returns `NA_real`.

## Details

A near-certain event has an s-value near 0 because it is similar to
getting 0 successful coin flips out of 0 tosses, which is certain and
unsurprising.An event with a probability of 0.5 is as surprising as
getting a successful coin toss.A near-impossible event has a very large
s-value because its occurrence would be extremely surprising, like
observing many consecutive successes on a fair coin.When
`skeptical = TRUE` (default), a ceiling of \\\log_2(n + 1)\\ is applied
to the s-value to avoid s-values of `Inf` when all samples are on one
side of the threshold. When `skeptical = FALSE`, s-values of `Inf` are
allowed.

## Functions

- `svalue()`: Calculate an s-value from a posterior distribution.

- `p2svalue()`: Calculate an s-value from a vector of probabilities.

## References

Greenland, S. 2019. Valid P-Values Behave Exactly as They Should: Some
Misleading Criticisms of P-Values and Their Resolution With S-Values.
The American Statistician 73(sup1): 106–114.
[doi:10.1080/00031305.2018.1529625](https://doi.org/10.1080/00031305.2018.1529625)
.

## See also

Other summary:
[`direction()`](https://poissonconsulting.github.io/extras/dev/reference/direction.md),
[`directional_information()`](https://poissonconsulting.github.io/extras/dev/reference/directional-information.md),
[`kurtosis()`](https://poissonconsulting.github.io/extras/dev/reference/kurtosis.md),
[`lower()`](https://poissonconsulting.github.io/extras/dev/reference/lower.md),
[`probability_direction()`](https://poissonconsulting.github.io/extras/dev/reference/probability_direction.md),
[`pvalue()`](https://poissonconsulting.github.io/extras/dev/reference/pvalue.md),
[`pzeros()`](https://poissonconsulting.github.io/extras/dev/reference/pzeros.md),
[`skewness()`](https://poissonconsulting.github.io/extras/dev/reference/skewness.md),
[`upper()`](https://poissonconsulting.github.io/extras/dev/reference/upper.md),
[`variance()`](https://poissonconsulting.github.io/extras/dev/reference/variance.md),
[`xtr_mean()`](https://poissonconsulting.github.io/extras/dev/reference/xtr_mean.md),
[`xtr_median()`](https://poissonconsulting.github.io/extras/dev/reference/xtr_median.md),
[`xtr_sd()`](https://poissonconsulting.github.io/extras/dev/reference/xtr_sd.md),
[`zeros()`](https://poissonconsulting.github.io/extras/dev/reference/zeros.md),
[`zscore()`](https://poissonconsulting.github.io/extras/dev/reference/zscore.md)

## Examples

``` r
svalue(as.numeric(0:100))
#> [1] 6.658211
svalue(as.numeric(0:100), side = "left")
#> [1] 6.658211
svalue(as.numeric(0:100), side = "right")
#> [1] 0
svalue(rnorm(1e4, mean = 1), side = "left")
#> [1] 2.654717
svalue(rnorm(1e4, mean = 1), side = "right")
#> [1] 0.2412704
svalue(rep(1, 10)) # skeptical = TRUE (default) avoids Inf
#> [1] 3.459432
svalue(rep(1, 10), skeptical = FALSE) # skeptical = FALSE allows Inf
#> [1] Inf

p2svalue(seq(0, 1, by = 0.1))
#>  [1]       Inf 3.3219281 2.3219281 1.7369656 1.3219281 1.0000000 0.7369656
#>  [8] 0.5145732 0.3219281 0.1520031 0.0000000
```
