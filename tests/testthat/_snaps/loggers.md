# null logger works

    Code
      rs(logger("error", error_cnd(message = "error test")))
    Output
      <error/rlang_error>
      Error:
      ! error test

---

    Code
      rs(logger("warning", warning_cnd(message = "warning test")))
    Message
      Warning:
      warning test

---

    Code
      rs(logger("message", message_cnd(message = "message test")))
    Output
      message test

# console logger works

    Code
      rs(logger("error", "error test"))
    Output
      error: error test

---

    Code
      rs(logger("warning", "warning test"))
    Output
      warning: warning test

---

    Code
      rs(logger("info", "info test"))
    Output
      info: info test

---

    Code
      rs(logger("request", "request test", req))
    Output
      request: request test

---

    Code
      rs(logger("error", catch_cnd(stop("error test"))))
    Output
      error: error test

---

    Code
      rs(logger("warning", catch_cnd(warning("warning test"))))
    Output
      warning: warning test

---

    Code
      rs(logger("message", catch_cnd(message("message test"))))
    Output
      message: message test

---

    Code
      rs(logger("info", catch_cnd(cnd("info", message = "info test"))))

---

    Code
      rs(logger("error", catch_cnd(reqres::abort_bad_request("http problem",
        instance = "test"))))
    Output
      error:  <instance test>
      error: http problem
      error: </instance>

# switch logger works

    Code
      rs(logger("test", "test test"))
    Output
      test: test test

---

    Code
      rs(logger("info", "info test"))
    Output
      info: info test

---

    Code
      rs(logger("warning", warning_cnd(message = "warning test")))
    Message
      Warning:
      warning test

# logger logger works

    Code
      rs(logger("error", "error test"))
    Output
      ERROR [2025-02-26 08:18:18] error: error test}

---

    Code
      rs(logger("warning", "warning test"))
    Output
      WARN [2025-02-26 08:18:18] warning: warning test}

---

    Code
      rs(logger("info", "info test"))
    Output
      INFO [2025-02-26 08:18:18] info: info test}

---

    Code
      rs(logger("request", "request test", req))
    Output
      SUCCESS [2025-02-26 08:18:18] request: request test}

---

    Code
      rs(logger(logger::INFO, "info test"))
    Output
      INFO [2025-02-26 08:18:18] info test}

---

    Code
      rs(logger(1, "info test"))
    Output
      INFO [2025-02-26 08:18:18] info test}

