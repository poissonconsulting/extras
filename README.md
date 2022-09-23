
<!-- README.md is generated from README.Rmd. Please edit that file -->

# extras <img src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/poissonconsulting/extras/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/poissonconsulting/extras/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/poissonconsulting/extras/branch/master/graph/badge.svg)](https://app.codecov.io/gh/poissonconsulting/extras?branch=master)
[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![CRAN
status](https://www.r-pkg.org/badges/version/extras)](https://cran.r-project.org/package=extras)
![CRAN downloads](https://cranlogs.r-pkg.org/badges/extras)
<!-- badges: end -->

`extras` provides helper functions for Bayesian analyses.

In particular it provides functions to numericise R objects (coerce to
numeric objects), summarise MCMC (Monte Carlo Markov Chain) samples and
calculate deviance residuals as well as R translations of some BUGS
(Bayesian Using Gibbs Sampling), JAGS (Just Another Gibbs Sampler), STAN
and TMB (Template Model Builder) functions.

## Installation

<!-- To install the latest release from [CRAN](https://cran.r-project.org) -->

To install the developmental version from
[GitHub](https://github.com/poissonconsulting/extras)

``` r
# install.packages("remotes")
remotes::install_github("poissonconsulting/extras")
```

## Demonstration

### Numericise R Objects

Atomic vectors, matrices, arrays and data.frames of appropriate classes
can be converted to numeric objects suitable for Bayesian analysis using
the `numericise()` (and `numericize()`) function.

``` r
library(extras)
#> 
#> Attaching package: 'extras'
#> The following object is masked from 'package:stats':
#> 
#>     step
numericise(
  data.frame(logical = c(TRUE, FALSE),
             factor = factor(c("blue", "green")),
             Date = as.Date(c("2000-01-01", "2000-01-02")),
             hms = hms::as_hms(c("00:00:02", "00:01:01"))
  )
)
#>      logical factor  Date hms
#> [1,]       1      1 10957   2
#> [2,]       0      2 10958  61
```

### Summarise MCMC Samples

The `extras` package provides functions to summarise MCMC samples like
`svalue()` which gives the *surprisal value* (Greenland, 2019)

``` r
set.seed(1)
x <- rnorm(100)
svalue(rnorm(100))
#> [1] 0.3183615
svalue(rnorm(100, mean = 1))
#> [1] 1.704015
svalue(rnorm(100, mean = 2))
#> [1] 3.850857
svalue(rnorm(100, mean = 3))
#> [1] 5.073249
```

## R translations

The package also provides R translations of `BUGS` (and `JAGS`)
functions such as `pow()` and `log<-`.

``` r
pow(10, 2)
#> [1] 100

mu <- NULL
log(mu) <- 1
mu
#> [1] 2.718282
```

## References

Greenland, S. 2019. Valid P -Values Behave Exactly as They Should: Some
Misleading Criticisms of P -Values and Their Resolution With S -Values.
The American Statistician 73(sup1): 106–114.
<https://doi.org/10.1080/00031305.2018.1529625>.

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/extras/issues).

[Pull requests](https://github.com/poissonconsulting/extras/pulls) are
always welcome.

## Code of Conduct

Please note that the extras project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
