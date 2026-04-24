# Log Base 2 Transformation

Replaces a object with the base 2 exponent of value.

## Usage

``` r
log2(x) <- value
```

## Arguments

- x:

  An object.

- value:

  A numeric atomic object.

## Value

Called for the side effect of updating `x`.

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
[`ilog2()`](https://poissonconsulting.github.io/extras/dev/reference/ilog2.md),
[`ilogit()`](https://poissonconsulting.github.io/extras/dev/reference/ilogit.md),
[`inv_logit()`](https://poissonconsulting.github.io/extras/dev/reference/inv_logit.md),
[`invlogit()`](https://poissonconsulting.github.io/extras/dev/reference/invlogit.md),
`log10<-()`, `log<-()`,
[`logit()`](https://poissonconsulting.github.io/extras/dev/reference/logit.md),
`logit<-()`,
[`phi()`](https://poissonconsulting.github.io/extras/dev/reference/phi.md),
[`pow()`](https://poissonconsulting.github.io/extras/dev/reference/pow.md),
[`step()`](https://poissonconsulting.github.io/extras/dev/reference/step.md)

## Examples

``` r
x <- NULL
log2(x) <- c(0.5, 5)
x
#> [1]  1.414214 32.000000
```
