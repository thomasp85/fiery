# null logger works

    Code
      logger("error", "error test")
    Message
      error: error test

---

    Code
      logger("warning", "warning test")
    Message
      warning: warning test

---

    Code
      logger("message", "message test")
    Message
      message: message test

# console logger works

    Code
      logger("error", "error test")
    Output
      error: error test

---

    Code
      logger("warning", "warning test")
    Output
      warning: warning test

---

    Code
      logger("info", "info test")
    Output
      info: info test

---

    Code
      logger("request", "request test", req)
    Output
      request: request test

# switch logger works

    Code
      logger("test", "test test")
    Output
      test: test test

---

    Code
      logger("info", "info test")
    Output
      info: info test

---

    Code
      logger("warning", "warning test")
    Message
      warning: warning test

# logger logger works

    Code
      logger("error", "error test")
    Output
      ERROR [2025-02-26 08:18:18] error test}

---

    Code
      logger("warning", "warning test")
    Output
      WARN [2025-02-26 08:18:18] warning test}

---

    Code
      logger("info", "info test")
    Output
      INFO [2025-02-26 08:18:18] info test}

---

    Code
      logger("request", "request test", req)
    Output
      SUCCESS [2025-02-26 08:18:18] request test}

