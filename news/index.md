# Changelog

## extras 0.10.0

- Add
  [`probability_direction()`](https://poissonconsulting.github.io/extras/reference/probability_direction.md)
  and
  [`directional_information()`](https://poissonconsulting.github.io/extras/reference/directional-information.md)
  ([\#106](https://github.com/poissonconsulting/extras/issues/106)).
- Add functions to calculate s-values and information from vectors of
  probabilities:
  [`p2svalue()`](https://poissonconsulting.github.io/extras/reference/svalue.md)
  and
  [`p2info()`](https://poissonconsulting.github.io/extras/reference/directional-information.md)
  ([\#122](https://github.com/poissonconsulting/extras/issues/122)).
- Add functions to calculate the Region of Practical Equivalence (ROPE)
  and credible intervals:
  [`xtr_ci()`](https://poissonconsulting.github.io/extras/reference/xtr_ci.md),
  [`xtr_ci_eti()`](https://poissonconsulting.github.io/extras/reference/xtr_ci_eti.md),
  [`xtr_ci_hdi()`](https://poissonconsulting.github.io/extras/reference/xtr_ci_hdi.md),
  [`xtr_rope()`](https://poissonconsulting.github.io/extras/reference/xtr_rope.md)
  ([\#110](https://github.com/poissonconsulting/extras/issues/110)).
- Add skew-lognormal distribution family
  ([`dskewlnorm()`](https://poissonconsulting.github.io/extras/reference/skewlnorm.md),
  [`pskewlnorm()`](https://poissonconsulting.github.io/extras/reference/skewlnorm.md),
  [`qskewlnorm()`](https://poissonconsulting.github.io/extras/reference/skewlnorm.md),
  [`rskewlnorm()`](https://poissonconsulting.github.io/extras/reference/skewlnorm.md),
  [`ran_skewlnorm()`](https://poissonconsulting.github.io/extras/reference/ran_skewlnorm.md),
  [`sens_skewlnorm()`](https://poissonconsulting.github.io/extras/reference/sens_skewlnorm.md),
  [`quant_skewlnorm()`](https://poissonconsulting.github.io/extras/reference/quant_skewlnorm.md),
  [`dev_skewlnorm()`](https://poissonconsulting.github.io/extras/reference/dev_skewlnorm.md),
  [`res_skewlnorm()`](https://poissonconsulting.github.io/extras/reference/res_skewlnorm.md),
  [`log_lik_skewlnorm()`](https://poissonconsulting.github.io/extras/reference/log_lik_skewlnorm.md),
  [`prob_skewlnorm()`](https://poissonconsulting.github.io/extras/reference/prob_skewlnorm.md))
  ([\#127](https://github.com/poissonconsulting/extras/issues/127)).
- Add `skeptical` argument to
  [`pvalue()`](https://poissonconsulting.github.io/extras/reference/pvalue.md),
  [`svalue()`](https://poissonconsulting.github.io/extras/reference/svalue.md),
  and
  [`directional_information()`](https://poissonconsulting.github.io/extras/reference/directional-information.md),
  defaulting to `TRUE`
  ([\#133](https://github.com/poissonconsulting/extras/issues/133)).
- Advance deprecation of
  [`as_list_unnamed()`](https://poissonconsulting.github.io/extras/reference/as_list_unnamed.md),
  [`pextreme()`](https://poissonconsulting.github.io/extras/reference/pextreme.md),
  and
  [`sextreme()`](https://poissonconsulting.github.io/extras/reference/sextreme.md)
  from soft-deprecated to warnings
  ([\#140](https://github.com/poissonconsulting/extras/issues/140)).

## extras 0.9.0

- Add
  [`log_lik_exp()`](https://poissonconsulting.github.io/extras/reference/log_lik_exp.md),
  [`log_lik_beta()`](https://poissonconsulting.github.io/extras/reference/log_lik_beta.md),
  and
  [`log_lik_unif()`](https://poissonconsulting.github.io/extras/reference/log_lik_unif.md)
  ([\#93](https://github.com/poissonconsulting/extras/issues/93),
  [\#94](https://github.com/poissonconsulting/extras/issues/94),
  [\#95](https://github.com/poissonconsulting/extras/issues/95)).
- Allow one-sided p-values and s-values via the `side` argument of
  [`pvalue()`](https://poissonconsulting.github.io/extras/reference/pvalue.md)
  and
  [`svalue()`](https://poissonconsulting.github.io/extras/reference/svalue.md);
  tighten
  [`pvalue()`](https://poissonconsulting.github.io/extras/reference/pvalue.md)’s
  sample-size correction to only apply when it would otherwise return
  exactly 0
  ([\#100](https://github.com/poissonconsulting/extras/issues/100),
  [\#105](https://github.com/poissonconsulting/extras/issues/105)).
- Add truncation arguments (`tlower`, `tupper`) to `log_lik_*()`
  functions; add `prob_*()` and `quant_*()` function families
  ([\#119](https://github.com/poissonconsulting/extras/issues/119)).

## extras 0.8.0

CRAN release: 2025-01-13

- Added a scalar case to
  [`log_lik_beta_binom()`](https://poissonconsulting.github.io/extras/reference/log_lik_beta_binom.md)
  to improve speed for scalar inputs.
- Add memoization (if memoize package is installed) and data has \> 800
  rows to gain speed from repeated function calls.
- Use a vectorized optimization to improve speed of optimization
  required for deviance calculation.
- Remove dependency on MASS package so minimum R version can be brought
  down to 4.0.0 from 4.3.0.

## extras 0.7.3

CRAN release: 2024-08-27

- Ensure all tests, examples, and vignettes run conditionally for
  packages listed in suggests.

## extras 0.7.2

CRAN release: 2024-08-22

- Fix behaviour of
  [`dev_beta_binom()`](https://poissonconsulting.github.io/extras/reference/dev_beta_binom.md)
  when size \< x.

## extras 0.7.1

- Refactored
  [`log_lik_beta_binom()`](https://poissonconsulting.github.io/extras/reference/log_lik_beta_binom.md)
  to speed up optimization required in
  [`dev_beta_binom()`](https://poissonconsulting.github.io/extras/reference/dev_beta_binom.md).
- Styled all code in package.

## extras 0.7.0

- Added sensitivity functions to produce list of new parameters that
  vary standard deviations (`sens_xxx()`).
- Added Skew Normal family of functions
  ([`ran_skewnorm()`](https://poissonconsulting.github.io/extras/reference/ran_skewnorm.md),
  [`log_lik_skewnorm()`](https://poissonconsulting.github.io/extras/reference/log_lik_skewnorm.md),
  [`res_skewnorm()`](https://poissonconsulting.github.io/extras/reference/res_skewnorm.md),
  [`dev_skewnorm()`](https://poissonconsulting.github.io/extras/reference/dev_skewnorm.md)).
- Added Skew Normal distribution functions
  ([`rskewnorm()`](https://poissonconsulting.github.io/extras/reference/skewnorm.md),
  [`dskewnorm()`](https://poissonconsulting.github.io/extras/reference/skewnorm.md),
  [`pskewnorm()`](https://poissonconsulting.github.io/extras/reference/skewnorm.md),
  [`qskewnorm()`](https://poissonconsulting.github.io/extras/reference/skewnorm.md)).
- Added `log2()<-`, `log10()<-`,
  [`ilog2()`](https://poissonconsulting.github.io/extras/reference/ilog2.md),
  and
  [`ilog10()`](https://poissonconsulting.github.io/extras/reference/ilog10.md)
  functions.
- Added
  [`exp2()`](https://poissonconsulting.github.io/extras/reference/exp2.md)
  and
  [`exp10()`](https://poissonconsulting.github.io/extras/reference/exp10.md)
  functions.

## extras 0.6.1

CRAN release: 2023-05-10

- Fixed M1mac test

## extras 0.6.0

CRAN release: 2023-04-28

### New Features

- Added family of functions for the following distributions

  - `xx_gamma()`
  - `xx_student()`
  - `xx_beta_binomial()`

- Added standardized residuals to residual functions for all
  distributions

### Bug Fixes

- Exported
  [`pbern()`](https://poissonconsulting.github.io/extras/reference/bern.md),
  [`qbern()`](https://poissonconsulting.github.io/extras/reference/bern.md)
  and
  [`rbern()`](https://poissonconsulting.github.io/extras/reference/bern.md).

### Internal

- Fix for dev testthat
- Fix tests for dev waldo

## extras 0.5.0

CRAN release: 2022-10-30

- Added following zero-inflated gamma Poisson functions
  - [`log_lik_gamma_pois_zi()`](https://poissonconsulting.github.io/extras/reference/log_lik_gamma_pois_zi.md)
  - [`ran_gamma_pois_zi()`](https://poissonconsulting.github.io/extras/reference/ran_gamma_pois_zi.md)
  - [`dev_gamma_pois_zi()`](https://poissonconsulting.github.io/extras/reference/dev_gamma_pois_zi.md)
  - [`res_gamma_pois_zi()`](https://poissonconsulting.github.io/extras/reference/res_gamma_pois_zi.md)
    Fixed bugs in
  - [`dev_binom()`](https://poissonconsulting.github.io/extras/reference/dev_binom.md)
  - [`dev_pois_zi()`](https://poissonconsulting.github.io/extras/reference/dev_pois_zi.md)
- Set R \>= 3.5

## extras 0.4.0

CRAN release: 2022-09-23

- Added
  [`log_odds()`](https://poissonconsulting.github.io/extras/reference/log_odds.md)
  and `log_odds()<-`.
- Added
  [`dbern()`](https://poissonconsulting.github.io/extras/reference/bern.md),
  [`pbern()`](https://poissonconsulting.github.io/extras/reference/bern.md),
  [`qbern()`](https://poissonconsulting.github.io/extras/reference/bern.md),
  [`rbern()`](https://poissonconsulting.github.io/extras/reference/bern.md).

## extras 0.3.0

CRAN release: 2022-06-08

### Added

- Added the following functions
- [`step()`](https://poissonconsulting.github.io/extras/reference/step.md)
  R version of JAGS function.
- [`ilog()`](https://poissonconsulting.github.io/extras/reference/ilog.md).
- [`inv_logit()`](https://poissonconsulting.github.io/extras/reference/inv_logit.md).
- [`inv_odds()`](https://poissonconsulting.github.io/extras/reference/inv_odds.md)
  and `odds()<-`.
- [`odds_ratio2()`](https://poissonconsulting.github.io/extras/reference/odds_ratio2.md)
  and
  [`log_odds_ratio2()`](https://poissonconsulting.github.io/extras/reference/log_odds_ratio2.md).
- [`proportional_change()`](https://poissonconsulting.github.io/extras/reference/proportional_change.md),
  [`proportional_difference()`](https://poissonconsulting.github.io/extras/reference/proportional_difference.md).
- [`ran_pois_zi()`](https://poissonconsulting.github.io/extras/reference/ran_pois_zi.md),
  [`log_lik_pois_zi()`](https://poissonconsulting.github.io/extras/reference/log_lik_pois_zi.md),
  [`res_pois_zi()`](https://poissonconsulting.github.io/extras/reference/res_pois_zi.md)
  and
  [`dev_pois_zi()`](https://poissonconsulting.github.io/extras/reference/dev_pois_zi.md).
- [`pzeros()`](https://poissonconsulting.github.io/extras/reference/pzeros.md)
  to calculate proportion of zeros in a numeric object.

### Modifications

- `res_xx()` functions now return data (actual or simulated) if
  `type = 'data'`.

### Bug Fixes

- Fixed normal and log-normal deviances which were out by a factor of 2
  and 1.41 respectively.

## extras 0.2.0

CRAN release: 2021-08-05

### Added

#### Functions

- Added the following function families
  - `ran_xx()` (random samples)
  - `log_lik_xx()` (log-likelihood)
  - `dev_xx()` (deviances)
  - `res_xx()` (residuals)

for the following distributions - `_bern`, (Bernoulli), - `_binom`
(binomial) - `_pois` (Poisson) - `_norm` (normal) - `_lnorm`
(log-normal) - `_gamma` (gamma) - `_gamma_pois` (gamma-Poisson)

- Added
  [`odds()`](https://poissonconsulting.github.io/extras/reference/odds.md),
  `odd_ratio()` and
  [`log_odds_ratio()`](https://poissonconsulting.github.io/extras/reference/log_odds_ratio.md)
  to calculate odds, odds ratio and log odds ratio.
- Added moments functions
  [`xtr_mean()`](https://poissonconsulting.github.io/extras/reference/xtr_mean.md),
  [`xtr_sd()`](https://poissonconsulting.github.io/extras/reference/xtr_sd.md),
  [`variance()`](https://poissonconsulting.github.io/extras/reference/variance.md),
  [`skewness()`](https://poissonconsulting.github.io/extras/reference/skewness.md)
  and
  [`kurtosis()`](https://poissonconsulting.github.io/extras/reference/kurtosis.md).
- Added
  [`xtr_median()`](https://poissonconsulting.github.io/extras/reference/xtr_median.md)
  to calculate the median.
- Added
  [`invlogit()`](https://poissonconsulting.github.io/extras/reference/invlogit.md)
  to calculate the inverse logistic transform.
- Added
  [`fabs()`](https://poissonconsulting.github.io/extras/reference/fabs.md)
  to calculate the absolute value of x. Used in TMB as replacement for
  [`abs()`](https://rdrr.io/r/base/MathFun.html).
- Added
  [`zeros()`](https://poissonconsulting.github.io/extras/reference/zeros.md)
  function to sum number of 0 values in a numeric (or MCMC object).

#### Arguments

- Added `threshold = 0` argument to
  [`pvalue()`](https://poissonconsulting.github.io/extras/reference/pvalue.md)
  and
  [`svalue()`](https://poissonconsulting.github.io/extras/reference/svalue.md).
- Added `na_rm = FALSE` argument to
  [`lower()`](https://poissonconsulting.github.io/extras/reference/lower.md),
  [`upper()`](https://poissonconsulting.github.io/extras/reference/upper.md),
  [`pvalue()`](https://poissonconsulting.github.io/extras/reference/pvalue.md),
  [`svalue()`](https://poissonconsulting.github.io/extras/reference/svalue.md)
  and
  [`zscore()`](https://poissonconsulting.github.io/extras/reference/zscore.md).

### Deprecated

- Soft-deprecated
  [`as_list_unnamed()`](https://poissonconsulting.github.io/extras/reference/as_list_unnamed.md)
  for
  [`as_list()`](https://poissonconsulting.github.io/extras/reference/as_list.md).
- Soft-deprecated
  [`pextreme()`](https://poissonconsulting.github.io/extras/reference/pextreme.md)
  and
  [`sextreme()`](https://poissonconsulting.github.io/extras/reference/sextreme.md).

## extras 0.1.0

CRAN release: 2020-09-26

- Added
  [`chk_indices()`](https://poissonconsulting.github.io/extras/reference/chk_indices.md)
  and
  [`vld_indices()`](https://poissonconsulting.github.io/extras/reference/chk_indices.md)
  to check (validate) if an object is a list of indices ie a vector of
  one or more positive integer values.
- Added
  [`par_pattern()`](https://poissonconsulting.github.io/extras/reference/par_pattern.md)
  to provide string of regular expression for a parameter name.
- Added
  [`as_list_unnamed()`](https://poissonconsulting.github.io/extras/reference/as_list_unnamed.md)
  generic which by default strips all attributes except names.
- Modified
  [`chk_pars()`](https://poissonconsulting.github.io/extras/reference/chk_pars.md)
  and
  [`vld_pars()`](https://poissonconsulting.github.io/extras/reference/chk_pars.md)
  to permit missing values and duplicates.

## extras 0.0.1

CRAN release: 2020-06-16

- Initial submission to CRAN.
