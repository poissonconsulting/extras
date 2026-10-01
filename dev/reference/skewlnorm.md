# Skew-Lognormal Distribution

The skew-lognormal distribution of a random variable whose natural
logarithm follows a
[Skew-Normal](https://poissonconsulting.github.io/extras/dev/reference/skewnorm.md)
distribution with location `locationlog`, scale `scalelog` and
`shapelog`. It reduces to the Log-Normal distribution when
`shapelog = 0`.

## Usage

``` r
dskewlnorm(
  x,
  locationlog = 0,
  scalelog = 1,
  shapelog = 0,
  log = FALSE,
  ...,
  meanlog,
  sdlog,
  shape
)

pskewlnorm(
  q,
  locationlog = 0,
  scalelog = 1,
  shapelog = 0,
  ...,
  meanlog,
  sdlog,
  shape
)

qskewlnorm(
  p,
  locationlog = 0,
  scalelog = 1,
  shapelog = 0,
  ...,
  meanlog,
  sdlog,
  shape
)

rskewlnorm(
  n = 1,
  locationlog = 0,
  scalelog = 1,
  shapelog = 0,
  ...,
  meanlog,
  sdlog,
  shape
)
```

## Arguments

- x:

  A numeric vector of values.

- locationlog:

  A numeric vector of location parameters of `log(x)`.

- scalelog:

  A non-negative numeric vector of scale parameters of `log(x)`.

- shapelog:

  A numeric vector of shape parameters of `log(x)`. Negative values
  result in leftward skew, while positive values result in rightward
  skew.

- log:

  A flag specifying whether to return the log-transformed value.

- ...:

  Unused.

- meanlog:

  **\[deprecated\]** A numeric vector of location parameters of
  `log(x)`. Described as "a numeric vector of the means on the log
  scale" prior to v. 0.10.1. Will be removed in a future version.

- sdlog:

  **\[deprecated\]** A non-negative numeric vector of scale parameters
  of `log(x)`. Described as "a non-negative numeric vector of the
  standard deviations on the log scale" prior to v. 0.10.1. Will be
  removed in a future version.

- shape:

  **\[deprecated\]** A numeric vector of shape parameters of `log(x)`.
  Will be removed in a future version.

- q:

  A vector of quantiles.

- p:

  A numeric vector of probabilities.

- n:

  A non-negative whole number of the number of random samples to
  generate.

## Value

`dskewlnorm` gives the density, `pskewlnorm` gives the distribution
function, `qskewlnorm` gives the quantile function, and `rskewlnorm`
generates random deviates. `pskewlnorm` and `qskewlnorm` use the lower
tail probability.

## Examples

``` r
dskewlnorm(x = 1:5, locationlog = 0, scalelog = 1, shapelog = 0.1)
#> [1] 0.39894228 0.16554302 0.07909057 0.04236014 0.02464460
dskewlnorm(x = 1:5, locationlog = 0, scalelog = 1, shapelog = -1)
#> [1] 0.398942280 0.076588593 0.019777519 0.006320389 0.002349402
qskewlnorm(p = c(0.1, 0.4), locationlog = 0, scalelog = 1, shapelog = 0.1)
#> [1] 0.3017701 0.8409793
qskewlnorm(p = c(0.1, 0.4), locationlog = 0, scalelog = 1, shapelog = -1)
#> [1] 0.1954953 0.4704475
pskewlnorm(q = 1:5, locationlog = 0, scalelog = 1, shapelog = 0.1)
#> [1] 0.4682745 0.7309608 0.8467152 0.9050737 0.9375887
pskewlnorm(q = 1:5, locationlog = 0, scalelog = 1, shapelog = -1)
#> [1] 0.7500000 0.9404110 0.9815125 0.9931394 0.9971098
rskewlnorm(n = 3, locationlog = 0, scalelog = 1, shapelog = 0.1)
#> [1] 1.308422 1.327854 1.441730
rskewlnorm(n = 3, locationlog = 0, scalelog = 1, shapelog = -1)
#> [1] 1.1844416 0.3525472 2.8623614
```
