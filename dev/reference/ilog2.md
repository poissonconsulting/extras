# Inverse Log Base 2 Transformation

Inverse log transforms a numeric atomic object with base 2.

## Usage

``` r
ilog2(x)
```

## Arguments

- x:

  An object.

## Value

A numeric atomic object.

## Details

A wrapper on
[`exp2`](https://poissonconsulting.github.io/extras/dev/reference/exp2.md)`(value)`.

## See also

Other translations:
[`exp10()`](https://poissonconsulting.github.io/extras/dev/reference/exp10.md),
[`exp2()`](https://poissonconsulting.github.io/extras/dev/reference/exp2.md),
[`fabs()`](https://poissonconsulting.github.io/extras/dev/reference/fabs.md),
[`ilog()`](https://poissonconsulting.github.io/extras/dev/reference/ilog.md),
[`ilog10()`](https://poissonconsulting.github.io/extras/dev/reference/ilog10.md),
[`ilogit()`](https://poissonconsulting.github.io/extras/dev/reference/ilogit.md),
[`inv_logit()`](https://poissonconsulting.github.io/extras/dev/reference/inv_logit.md),
[`invlogit()`](https://poissonconsulting.github.io/extras/dev/reference/invlogit.md),
`log10<-()`, `log2<-()`, `log<-()`,
[`logit()`](https://poissonconsulting.github.io/extras/dev/reference/logit.md),
`logit<-()`,
[`phi()`](https://poissonconsulting.github.io/extras/dev/reference/phi.md),
[`pow()`](https://poissonconsulting.github.io/extras/dev/reference/pow.md),
[`step()`](https://poissonconsulting.github.io/extras/dev/reference/step.md)

## Examples

``` r
x <- c(2, 4.5)
ilog2(x)
#> [1]  4.00000 22.62742
```
