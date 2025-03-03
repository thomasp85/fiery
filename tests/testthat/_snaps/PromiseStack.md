# DelayStack works

    Code
      rs(delay$eval())
    Output
      Message in `message()`:
      test

---

    Code
      rs(delay$eval())
    Output
      Message in `message()`:
      test2

---

    Code
      rs(delay$eval())
    Output
      <error/rlang_error>
      Error:
      ! error test

---

    Code
      rs(delay$eval())
    Output
      <error/rlang_error>
      Error in `private$calls[[i]]$then()`:
      ! error test

# TimeStack works

    Code
      rs(time$eval())
    Output
      Message in `message()`:
      test

---

    Code
      rs(time$eval())
    Output
      Message in `message()`:
      test

---

    Code
      rs(time$eval())
    Output
      Message in `message()`:
      test

---

    Code
      rs(time$eval())
    Output
      Message in `message()`:
      test

