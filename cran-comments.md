## Test environments

release 4.3.0

* OSX (local) - release
* OSX (actions) - release
* Ubuntu (actions) - 4.0 to 4.1, oldrel, release and devel
* Windows (actions) - release
* Windows (winbuilder) - devel

## R CMD check results

0 errors | 0 warnings | 0 notes

## CRAN Issues

Fixed the following issue on M1mac

  ══ Failed tests ════════════════════════════════════════════════════════════════
  ── Failure ('test-dev.R:79:3'): beta_binom ran ─────────────────────────────────
  mean(res) (`actual`) not equal to -0.00107466576791911 (`expected`).
  
    `actual`: -0.00107458
  `expected`: -0.00107467
  
  [ FAIL 1 | WARN 0 | SKIP 0 | PASS 1429 ]
  

## revdepcheck results

We checked 4 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
