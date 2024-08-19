## Test environments

release 4.4.1

* OSX (local) - release
* OSX (actions) - release
* OSX (R-hub) - devel
* Ubuntu (actions) - oldrel-1, release, and devel 
* Ubuntu (R-hub) - devel
* Fedora (R-hub)
* Windows (actions) - release
* Windows (winbuilder) - devel
* Windows (R-hub) - devel

## R CMD check results

0 errors | 0 warnings | 1 note

New maintainer:
  Nicole Hill <nicole@poissonconsulting.ca>
Old maintainer(s):
  Joe Thorley <joe@poissonconsulting.ca>
  
## CRAN Issues

Fixed the following error by conditionally skipping tests that rely on packages listed in suggests, if those packages are not installed.

══ Failed tests ════════════════════════════════════════════════════════════════
── Error ('test-log-lik.R:184:3'): beta_binom log_lik ──────────────────────────
<packageNotFoundError/error/condition>
Error in `loadNamespace(x)`: there is no package called 'aods3'

## revdepcheck results

We checked 4 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
