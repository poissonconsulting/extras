# Region of Practical Equivalence

Calculates the proportion of the posterior (or credible interval if
`level < 1`) that falls within the region of practical equivalence
(ROPE) to a `threshold`.

## Usage

``` r
xtr_rope(
  x,
  threshold = 0,
  interval = c(-0.1, 0.1),
  ...,
  level = 1,
  type = "HDI",
  na_rm = FALSE
)
```

## Arguments

- x:

  A numeric vector of MCMC samples.

- threshold:

  A number specifying the center of the ROPE.

- interval:

  A numeric vector of length 2 to be added to `threshold` to calculate
  the ROPE. Generally, `threshold[1] == - threshold[2]` and
  `threshold[1] < 0` should both be true.

- ...:

  Currently unused.

- level:

  A number \> 0 and \<= 1 specifying the probability coverage of the
  interval to use. The default of 1 uses the full posterior.

- type:

  A string indicating which type of CI to return. Currently allows
  Highest Density Intervals (`"HDI"`; default) and Equal-Tailed
  Intervals (`"ETI"`).

- na_rm:

  A flag indicating whether to remove missing values.

## Value

A number indicating the proportion of the posterior (or credible
interval) within the region of practical equivalence.

## Details

The ROPE is calculated as `threshold + interval`. Note that the default
is not appropriate for all models, since the `interval` is sensitive to
unit conversions.

## See also

[xtr_ci](https://poissonconsulting.github.io/extras/dev/reference/xtr_ci.md)

## Examples

``` r
xtr_rope(c(-Inf, -1, -0.1, 0.1, 1, Inf))
#> [1] 0.3333333
xtr_rope(rnorm(1e4))
#> [1] 0.0781
```
