# handlers can be added, triggered and removed

    Code
      rs(app$on("test", function(...) 10, id = "testid"))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! `id` must be unique. A handler with this id has already been added

# Fire objects are printed

    Code
      print(rs(app$format()))
    Output
      [1] "🔥 A fiery webserver"                    
      [2] "🔥  💥   💥   💥"                        
      [3] "🔥           Running on: 127.0.0.1:49925"
      [4] "🔥     Plugins attached: none"           
      [5] "🔥 Event handlers added: none"           

---

    Code
      print(rs(app$format()))
    Output
      [1] "🔥 A fiery webserver"                    
      [2] "🔥  💥   💥   💥"                        
      [3] "🔥           Running on: 127.0.0.1:49925"
      [4] "🔥     Plugins attached: test"           
      [5] "🔥 Event handlers added"                 
      [6] "🔥                start: 1"              
      [7] "🔥              request: 2"              

# protected events cannot be triggered

    Code
      rs(app$trigger(!!i))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! "start" and other protected events cannot be triggered manually

---

    Code
      rs(app$trigger(!!i))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! "resume" and other protected events cannot be triggered manually

---

    Code
      rs(app$trigger(!!i))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! "end" and other protected events cannot be triggered manually

---

    Code
      rs(app$trigger(!!i))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! "cycle-start" and other protected events cannot be triggered manually

---

    Code
      rs(app$trigger(!!i))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! "cycle-end" and other protected events cannot be triggered manually

---

    Code
      rs(app$trigger(!!i))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! "header" and other protected events cannot be triggered manually

---

    Code
      rs(app$trigger(!!i))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! "before-request" and other protected events cannot be triggered manually

---

    Code
      rs(app$trigger(!!i))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! "request" and other protected events cannot be triggered manually

---

    Code
      rs(app$trigger(!!i))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! "after-request" and other protected events cannot be triggered manually

---

    Code
      rs(app$trigger(!!i))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! "before-message" and other protected events cannot be triggered manually

---

    Code
      rs(app$trigger(!!i))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! "message" and other protected events cannot be triggered manually

---

    Code
      rs(app$trigger(!!i))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! "after-message" and other protected events cannot be triggered manually

---

    Code
      rs(app$trigger(!!i))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! "websocket-closed" and other protected events cannot be triggered manually

---

    Code
      rs(app$trigger(!!i))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! "send" and other protected events cannot be triggered manually

# data can be set, get and removed

    Code
      rs(app$get_data(1))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! `name` must be a single string, not the number 1.

---

    Code
      rs(app$get_data(c("test", "test2")))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! `name` must be a single string, not a character vector.

# plugins are being attached

    Code
      app$attach(plugin, 15)
    Condition
      Error in `app$attach()`:
      ! `name` must be a single string, not `NULL`.

---

    Code
      app$attach(plugin, 10)
    Condition
      Error in `app$attach()`:
      ! The `plugin` plugin is already loaded.
      i Use `force = TRUE` to reapply it.

---

    Code
      app$attach(plugin2)
    Condition
      Error in `app$attach()`:
      ! The `plugin2` plugin requires the following plugin: test

---

    Code
      app$attach(plugin2)
    Message
      test

---

    Code
      app$plugins$test <- plugin
    Condition
      Error:
      ! Use the `attach()` method to add plugins

---

    Code
      app$attach(plugin3)
    Condition
      Error in `app$attach()`:
      ! The `plugin3` plugin failed to attach to the app
      Caused by error in `plugin$on_attach()`:
      ! test

# id converter can be set and gets called

    Code
      app$set_client_id_converter("test")
    Condition
      Error in `app$set_client_id_converter()`:
      ! `converter` must be a function, not the string "test".

---

    Code
      app$set_client_id_converter(function(test) {
        10
      })
    Condition
      Error:
      ! `converter` must be a function containing the argument `request`

# active bindings work

    Code
      app$host <- 10
    Condition
      Error:
      ! `address` must be a single string, not the number 10.

---

    Code
      app$host <- letters[1:3]
    Condition
      Error:
      ! `address` must be a single string, not a character vector.

---

    Code
      app$port <- "test"
    Condition
      Error:
      ! `n` must be a whole number, not the string "test".

---

    Code
      app$port <- 1.5
    Condition
      Error:
      ! `n` must be a whole number, not the number 1.5.

---

    Code
      app$refresh_rate <- "test"
    Condition
      Error:
      ! `rate` must be a number, not the string "test".

---

    Code
      app$refresh_rate <- 1:5
    Condition
      Error:
      ! `rate` must be a number, not an integer vector.

---

    Code
      app$refresh_rate_nb <- "test"
    Condition
      Error:
      ! `rate` must be a number, not the string "test".

---

    Code
      app$refresh_rate_nb <- 1:5
    Condition
      Error:
      ! `rate` must be a number, not an integer vector.

---

    Code
      app$trigger_dir <- "test"
    Condition
      Error:
      ! `dir` must be a valid directory

---

    Code
      app$trigger_dir <- 1:5
    Condition
      Error:
      ! `dir` must be a single string or `NULL`, not an integer vector.

# errors in start and resume gets caught

    Code
      app$ignite(silent = TRUE, block = FALSE)
    Output
      error: Testing an error
    Code
      later::run_now()

---

    Code
      app$reignite(silent = TRUE, block = FALSE)
    Output
      error: Testing an error
    Code
      later::run_now()

# futures can be added and called

    Code
      app$ignite(silent = TRUE)
    Output
      message: 10

---

    Code
      app$ignite(silent = TRUE)
    Output
      message: 10

---

    Code
      app$ignite()
    Message
      Fire started at <127.0.0.1:49925>
    Output
      message: 10

# ignite is blocked during run

    Code
      app$ignite()
      later::run_now()
    Output
      warning: Server is already running and cannot be started

# external triggers are fired

    Code
      app$ignite(silent = TRUE, block = FALSE)
    Output
      warning: External triggers must be an rds file containing a list
    Code
      later::run_now()
      app$extinguish()

# websockets are attached, and removed

    Code
      app$test_websocket(req, "test", FALSE)
    Message
      test

---

    Code
      app$send("keep testing", session_id_cookie()(reqres::Request$new(req)))
    Message
      keep testing

---

    Code
      app$send("keep testing again")
    Message
      keep testing again

---

    Code
      app$close_ws_con(session_id_cookie()(reqres::Request$new(req)))
    Message
      closing

# showcase opens a browser

    Code
      app$ignite(showcase = TRUE)
    Message
      Fire started at <127.0.0.1:49925>
      Open browser at http://127.0.0.1:49925/

---

    Code
      app$ignite(showcase = "/test")
    Message
      Fire started at <127.0.0.1:49925>
      Open browser at http://127.0.0.1:49925/test

---

    Code
      app$ignite(showcase = TRUE, block = FALSE)
    Message
      Fire started at <127.0.0.1:49925>
      Open browser at http://127.0.0.1:49925/

# app can be mounted at path

    Code
      app$root <- 123
    Condition
      Error:
      ! `path` must be a single string, not the number 123.

---

    Code
      app$root <- c("test", "test2")
    Condition
      Error:
      ! `path` must be a single string, not a character vector.

---

    Code
      app$test_websocket(req, "test")
    Message
      test
      closing

---

    Code
      res <- app$test_request(req)
    Output
      error: URL (/testing) not matching mount point (/test)

---

    Code
      res <- app$test_header(req)
    Output
      error: URL (/testing) not matching mount point (/test)

---

    Code
      app$test_websocket(req, "test")
    Output
      error: URL (/testing) not matching mount point (/test)
    Message
      closing

# Logging can be configured

    Code
      res <- app$trigger("test")
    Output
      test: this is a test

---

    Code
      res <- app$test_request(fake_request("www.example.com/path", headers = list(
        Cookie = "fiery_id=ID_test")))
    Output
      request: 123.123.123.123 - ID_test [29/Jan/2025:08:17:44 +0100] "GET /path HTTP/1.1" 404
      request: 0

# is_running works

    Code
      app$ignite(silent = TRUE)
    Output
      message: TRUE

# safe_call catches conditions

    Code
      cnd <- self$safe_call(stop("error test"))
    Output
      error: error test

---

    Code
      cnd <- self$safe_call(warning("warning test"))
    Output
      warning: warning test

---

    Code
      cnd <- self$safe_call(message("message test"))
    Output
      message: message test

# requests are created with the correct settings

    Code
      app$key
    Condition
      Error:
      ! `key` can only be set, not retrieved

---

    Code
      app$key <- 500
    Condition
      Error:
      ! Malformed key. It must be provided as either a string, a raw vector or NULL

---

    Code
      app$key <- "xyz"
    Condition
      Error:
      ! Malformed key. If given as a string it must be hexadecimal encoded

---

    Code
      app$key <- "1234"
    Condition
      Error:
      ! Malformed key. The key must be 32 bit

---

    Code
      app$session_cookie_settings <- "reqres"
    Condition
      Error:
      ! `session_cookie_settings` can only be set to a valid settings object
      i Construct one using `reqres::session_cookie()`

---

    Code
      request <- app$.__enclos_env__$private$new_req(req)
    Condition
      Warning:
      Ignoring `session_cookie` argument when `key` is NULL

# request handlers handle conditions

    Code
      res <- app$test_request(req)
    Output
      error: test

---

    Code
      res <- app$test_request(req)
    Output
      error: test

---

    Code
      res <- app$test_request(req)
    Output
      error: Error formatting the response body
      error: Caused by error in `self$formatter()`:
      error: ! test

# header handlers handle conditions

    Code
      res <- app$test_header(req)
    Output
      error: test

---

    Code
      res <- app$test_header(req)
    Output
      error: test

---

    Code
      res <- app$test_header(req)
    Output
      error: Error formatting the response body
      error: Caused by error in `self$formatter()`:
      error: ! test

# static file serving works

    Code
      app$serve_static("/static", "test")
    Condition
      Error in `app$serve_static()`:
      ! `test` does not point to an existing file or directory

---

    Code
      app$serve_static("/static", getwd(), headers = 3)
    Condition
      Error in `app$serve_static()`:
      ! `headers` must be a named list, not the number 3.

---

    Code
      app$serve_static("/static", getwd(), headers = list(a = 4))
    Condition
      Error in `app$serve_static()`:
      ! `headers$a` must be a single string, not the number 4.

---

    Code
      app$serve_static("/static", getwd())
    Message
      Overwriting static url path /static

---

    Code
      app$exclude_static(4)
    Condition
      Error in `app$exclude_static()`:
      ! `at` must be a single string, not the number 4.

