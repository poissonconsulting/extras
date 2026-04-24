# Bernoulli Distribution

Bernoulli Distribution

## Usage

``` r
dbern(x, prob, log = FALSE)

pbern(q, prob, lower.tail = TRUE, log = FALSE)

qbern(p, prob, lower.tail = TRUE, log = FALSE)

rbern(n, prob)
```

## Arguments

- x:

  A vector of 0s and 1s.

- prob:

  A numeric vector of values between 0 and 1 of the probability of
  success.

- log:

  A flag specifying whether to return the log-transformed value.

- q:

  A vector of quantiles.

- lower.tail:

  A flag specifying whether to return the lower or upper tail of the
  distribution.

- p:

  A vector of probabilities.

- n:

  A non-negative whole number of the number of random samples to
  generate.

## Value

An numeric vector of the random samples.

## Examples

``` r
dbern(1, 0.5)
#> [1] 0.5
pbern(0.75, 0.5)
#> [1] 0.5
qbern(0.1, 0.5)
#> [1] 0
rbern(1, 0.5)
#> [1] 0
```
