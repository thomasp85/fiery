# DelayStack works

    Code
      delay$eval()
    Output
      Message in `message()`:
      test

---

    Code
      delay$eval()
    Output
      Message in `message()`:
      test2

---

    Code
      delay$eval()
    Output
      <error/rlang_error>
      Error:
      ! error test

---

    Code
      delay$eval()
    Output
      <error/rlang_error>
      Error in `private$calls[[i]]$then()`:
      ! error test

# TimeStack works

    Code
      time$eval()
    Output
      Message in `message()`:
      test

---

    Code
      time$eval()
    Output
      Message in `message()`:
      test

---

    Code
      time$eval()
    Output
      Message in `message()`:
      test

---

    Code
      time$eval()
    Output
      Message in `message()`:
      test

