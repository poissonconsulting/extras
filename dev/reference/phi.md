# Phi

The standard normal cumulative density function.

## Usage

``` r
phi(x)
```

## Arguments

- x:

  A numeric atomic object.

## Value

A numeric atomic object.

## Details

A wrapper on [`stats::pnorm()`](https://rdrr.io/r/stats/Normal.html).

## See also

Other translations:
[`exp10()`](https://poissonconsulting.github.io/extras/dev/reference/exp10.md),
[`exp2()`](https://poissonconsulting.github.io/extras/dev/reference/exp2.md),
[`fabs()`](https://poissonconsulting.github.io/extras/dev/reference/fabs.md),
[`ilog()`](https://poissonconsulting.github.io/extras/dev/reference/ilog.md),
[`ilog10()`](https://poissonconsulting.github.io/extras/dev/reference/ilog10.md),
[`ilog2()`](https://poissonconsulting.github.io/extras/dev/reference/ilog2.md),
[`ilogit()`](https://poissonconsulting.github.io/extras/dev/reference/ilogit.md),
[`inv_logit()`](https://poissonconsulting.github.io/extras/dev/reference/inv_logit.md),
[`invlogit()`](https://poissonconsulting.github.io/extras/dev/reference/invlogit.md),
`log10<-()`, `log2<-()`, `log<-()`,
[`logit()`](https://poissonconsulting.github.io/extras/dev/reference/logit.md),
`logit<-()`,
[`pow()`](https://poissonconsulting.github.io/extras/dev/reference/pow.md),
[`step()`](https://poissonconsulting.github.io/extras/dev/reference/step.md)

## Examples

``` r
phi(0:2)
#> [1] 0.5000000 0.8413447 0.9772499
```
