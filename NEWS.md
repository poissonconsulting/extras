# extras 0.1.0.9011

- Added `fabs()`.
- Added `invlogit()`.
- Added 'dev_neg_binom()` and `res_neg_binom()`.
- Added `log_lik_neg_binom()` and `log_lik_gamma_pois()`.
- Added random sample functions `ran_gamma_pois()` and `ran_neg_binom()`.

# extras 0.1.0.9010

- Internal changes only.


# extras 0.1.0.9009

- Renamed resample argument to simulate.
- Added `xtr_sd()`.


# extras 0.1.0.9008

- Soft-deprecated `as_list_unnamed()` for `as_list()`.
- Soft-deprecated `pextreme()` and `sextreme()`.
- Fixed bug in `ran_gamma_pois()`.
- Added tests for `ran_dist()` functions.
- Fixed bug in `res_lnorm()` for raw residuals.
- Added tests for `resample = TRUE` argument in `res_dist()` functions.
- Added random sample functions `ran_pois()`, `ran_norm()`, `ran_lnorm()`, `ran_binom()`, `ran_bern()`, `ran_gamma()` and `ran_pois_gamma()`.
- Added `resample = FALSE` argument to `res_dist()` functions.
- Set default values for all `dev_dist()` functions.
- Added `xtr_median()`.
- Added `na_rm = FALSE` argument to `lower()`, `upper()`, `pvalue()`, `svalue()` and `zscore()`.
- Added `threshold = 0` argument to `pvalue()` and `svalue()`.


# extras 0.1.0.9007

- Added moments functions `xtr_mean()`, `variance()`, `skewness()`, `kurtosis()`.
- Added zeros() function to sum number of 0 values in a numeric or mcmc object.


# extras 0.1.0.9006

- Added `dev_gamma_pois()` and `res_gamma_pois()`.
- Added `log_lik_bern()`, `log_lik_binom()`, `log_lik_pois()`, `log_lik_norm()` and `log_lik_lnorm()` to calculate log-likelihoods.


# extras 0.1.0.9005

- Renamed residual argument of dev_xx() functions to res.


# extras 0.1.0.9004

- Added `res_bern()`, `res_binom()`, `res_pois()`, `res_norm()` and `res_lnorm()` to calculate residuals.


# extras 0.1.0.9003

- Rename devxx() to dev_xx().


# extras 0.1.0.9002

- Added residual = FALSE argument to deviance functions.


# extras 0.1.0.9001

- Added `devbern()`, `devbinom()`, `devpois()`, `devnorm()` and `devlnorm()` to calculate deviance residuals.

# extras 0.1.0.9000

- Same as previous version.


# extras 0.1.0

- Added `chk_indices()` and `vld_indices()` to check (validate) if an object is a list of indices ie a vector of one or more positive integer values.
- Added `par_pattern()` to provide string of regular expression for a parameter name.
- Added `as_list_unnamed()` generic which by default strips all attributes except names.
- Modified `chk_pars()` and `vld_pars()` to permit missing values and duplicates.

# extras 0.0.1

- Initial submission to CRAN.
