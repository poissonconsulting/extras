## Test environments

release 4.0.0

* OSX (local) - release
* OSX (actions) - release and devel
* Ubuntu (actions) - 3.3 to release
* Windows (winbuilder) - devel
* Windows (actions) - release

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a resubmission of a new release.

> Please do not start your description field in the DESCRIPTION file with phrases like 'This is a R package', 'This package', the package name, the package title or similar.

> Please always explain all acronyms/abbreviations in the description text in the Description field of the DESCRIPTION file.

> Please always write non-English usage, package names, software names and API names in *undirected* single quotes in title and description in the DESCRIPTION file. e.g. --> 'R'

The DESCRIPTION description field has been updated accordingly to read

```
Description: Functions to numericise 'R' objects and summarise 'MCMC'
    (Monte Carlo Markov Chain) samples
    as well as 'R' translations of 'BUGS' (Bayesian Using Gibbs Sampling)
    and 'JAGS' (Just Another Gibbs Sampler) functions.
```

> Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means.
(If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar)

Added the following to `log<-` and `logit<-`

```
#' @return Called for the side effect of updating `x`.
```

> Please fix and resubmit, and document what was changed in the submission comments.

Done

