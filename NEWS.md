<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# extras 0.4.0.9000

- Same as previous version.


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
