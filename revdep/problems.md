# reqres

<details>

* Version: 0.2.5
* GitHub: https://github.com/thomasp85/reqres
* Source code: https://github.com/cran/reqres
* Date/Publication: 2022-08-19 12:50:03 UTC
* Number of recursive dependencies: 53

Run `revdepcheck::revdep_details(, "reqres")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘reqres-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Request
    > ### Title: HTTP Request Handling
    > ### Aliases: Request as.Request is.Request
    > 
    > ### ** Examples
    > 
    > fake_rook <- fiery::fake_request(
    ...
    +   content = 'This is an elaborate ruse',
    +   headers = list(
    +     Accept = 'application/json; text/*',
    +     Content_Type = 'text/plain'
    +   )
    + )
    Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
      namespace ‘reqres’ 0.2.5 is already loaded, but >= 0.2.5.9000 is required
    Calls: loadNamespace ... namespaceImportFrom -> asNamespace -> loadNamespace
    Execution halted
    ```

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       2.   ├─base::namespaceImportFrom(...)
       3.   │ └─base::asNamespace(ns)
       4.   └─base::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
      ── Error ('test-response.R:13:1'): (code run outside of `test_that()`) ─────────
      Error in `loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])`: namespace 'reqres' 0.2.5 is already loaded, but >= 0.2.5.9000 is required
      Backtrace:
          ▆
       1. └─base::loadNamespace(x) at test-response.R:13:1
       2.   ├─base::namespaceImportFrom(...)
       3.   │ └─base::asNamespace(ns)
       4.   └─base::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

