# DelayStack works

    Code
      delay$eval()
    Output
      <message/rlang_message>
      Message in `message()`:
      test

---

    Code
      delay$eval()
    Output
      <message/rlang_message>
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
      Error in `private$futures[[i]]$then()`:
      ! error test

# TimeStack works

    Code
      time$eval()
    Output
      <message/rlang_message>
      Message in `message()`:
      test

---

    Code
      time$eval()
    Output
      <message/rlang_message>
      Message in `message()`:
      test

---

    Code
      time$eval()
    Output
      <message/rlang_message>
      Message in `message()`:
      test

---

    Code
      time$eval()
    Output
      <message/rlang_message>
      Message in `message()`:
      test

