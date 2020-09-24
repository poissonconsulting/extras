# nlist

<details>

* Version: 0.2.0
* GitHub: https://github.com/poissonconsulting/nlist
* Source code: https://github.com/cran/nlist
* Date/Publication: 2020-06-25 20:20:03 UTC
* Number of recursive dependencies: 50

Run `revdep_details(, "nlist")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(nlist)
      Warning message:
      replacing previous import 'extras::as_list' by 'rlang::as_list' when loading 'term' 
      > 
      > test_check("nlist")
      [31m──[39m [31m1. Failure: [.nlist (@test-brackets.R#7) [39m [31m───────────────────────────────────[39m
      `nlist(x = 1, y = 2)[c(1, 1)]` did not throw an error.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 359 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: [.nlist (@test-brackets.R#7) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# term

<details>

* Version: 0.2.0
* GitHub: https://github.com/poissonconsulting/term
* Source code: https://github.com/cran/term
* Date/Publication: 2020-06-20 09:20:09 UTC
* Number of recursive dependencies: 40

Run `revdep_details(, "term")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(term)
      Warning message:
      replacing previous import 'extras::as_list' by 'rlang::as_list' when loading 'term' 
      > 
      > test_check("term")
      [31m──[39m [31m1. Failure: set_pars missing values (@test-set-pars.R#30) [39m [31m──────────────────[39m
      `set_pars(new_term(c("a [ 1]", "b")), c("b", NA))` did not throw an error.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 375 | SKIPPED: 2 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: set_pars missing values (@test-set-pars.R#30) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking whether package ‘term’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘extras::as_list’ by ‘rlang::as_list’ when loading ‘term’
    See ‘/Users/joe/Code/extras/revdep/checks.noindex/term/new/term.Rcheck/00install.out’ for details.
    ```

