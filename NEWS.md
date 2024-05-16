<!-- NEWS.md is maintained by https://fledge.cynkra.com, contributors should not edit this file -->

# extras 0.7.0

  - Added sensitivity functions to produce list of new parameters that vary standard deviations (`sens_xxx()`).
  - Added Skew Normal residual functions (`ran_skewnorm()`, `log_lik_skewnorm()`, `res_skewnorm()`, `dev_skewnorm()`).
  - Added Skew Normal distribution functions (`rskewnorm()`, `dskewnorm()`, `pskewnorm()`, `qskewnorm()`).
  - Added `log2()<-`, `log10()<-`, `ilog2()`, and `ilog10()` functions
  - Added `exp2()` and `exp10()` functions

# extras 0.6.1

- Fixed M1mac test

# extras 0.6.0

## New Features

- Added family of functions for the following distributions

  - `xx_gamma()`
  - `xx_student()`
  - `xx_beta_binomial()`

- Added standardized residuals to residual functions for all distributions

## Bug Fixes

- Exported `pbern()`, `qbern()` and `rbern()`.

## Internal

- Fix for dev testthat
- Fix tests for dev waldo


# extras 0.5.0

- Added following zero-inflated gamma Poisson functions
  - `log_lik_gamma_pois_zi()`
  - `ran_gamma_pois_zi()`
  - `dev_gamma_pois_zi()`
  - `res_gamma_pois_zi()`
Fixed bugs in 
  - `dev_binom()`
  - `dev_pois_zi()`
- Set R >= 3.5


# extras 0.4.0

- Added `log_odds()` and `log_odds()<-`.
- Added `dbern()`, `pbern()`, `qbern()`, `rbern()`.


# extras 0.3.0

## Added

- Added the following functions
- `step()` R version of JAGS function.
- `ilog()`.
- `inv_logit()`.
- `inv_odds()` and `odds()<-`.
- `odds_ratio2()` and `log_odds_ratio2()`.
- `proportional_change()`, `proportional_difference()`.
- `ran_pois_zi()`, `log_lik_pois_zi()`, `res_pois_zi()` and `dev_pois_zi()`.
- `pzeros()` to calculate proportion of zeros in a numeric object.

## Modifications

- `res_xx()` functions now return data (actual or simulated) if `type = 'data'`.

## Bug Fixes

- Fixed normal and log-normal deviances which were out by a factor of 2 and 1.41 respectively.

# extras 0.2.0

## Added 

### Functions

- Added the following function families 
  - `ran_xx()` (random samples)
  - `log_lik_xx()` (log-likelihood) 
  - `dev_xx()` (deviances)
  - `res_xx()` (residuals) 
  
for the following distributions
  - `_bern`, (Bernoulli), 
  - `_binom` (binomial) 
  - `_pois` (Poisson)
  - `_norm` (normal)
  - `_lnorm` (log-normal)
  - `_gamma` (gamma)
  - `_gamma_pois` (gamma-Poisson)

- Added `odds()`, `odd_ratio()` and `log_odds_ratio()` to calculate odds, odds ratio and log odds ratio.
- Added moments functions `xtr_mean()`, `xtr_sd()`, `variance()`, `skewness()` and `kurtosis()`.
- Added `xtr_median()` to calculate the median.
- Added `invlogit()` to calculate the inverse logistic transform.
- Added `fabs()` to calculate the absolute value of x. Used in TMB
as replacement for `abs()`.
- Added `zeros()` function to sum number of 0 values in a numeric (or MCMC object).

### Arguments

- Added `threshold = 0` argument to `pvalue()` and `svalue()`.
- Added `na_rm = FALSE` argument to `lower()`, `upper()`, `pvalue()`, `svalue()` and `zscore()`.

## Deprecated

- Soft-deprecated `as_list_unnamed()` for `as_list()`.
- Soft-deprecated `pextreme()` and `sextreme()`.

# extras 0.1.0

- Added `chk_indices()` and `vld_indices()` to check (validate) if an object is a list of indices ie a vector of one or more positive integer values.
- Added `par_pattern()` to provide string of regular expression for a parameter name.
- Added `as_list_unnamed()` generic which by default strips all attributes except names.
- Modified `chk_pars()` and `vld_pars()` to permit missing values and duplicates.

# extras 0.0.1

- Initial submission to CRAN.
