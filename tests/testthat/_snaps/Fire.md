# Fire objects are printed

    Code
      app$format()
    Output
      [1] "🔥 A fiery webserver"                    
      [2] "🔥  💥   💥   💥"                        
      [3] "🔥           Running on: 127.0.0.1:49925"
      [4] "🔥     Plugins attached: none"           
      [5] "🔥 Event handlers added: none"           

---

    Code
      app$format()
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
      app$trigger(i)
    Condition
      Error in `app$trigger()`:
      ! "start" and other protected events cannot be triggered manually

---

    Code
      app$trigger(i)
    Condition
      Error in `app$trigger()`:
      ! "resume" and other protected events cannot be triggered manually

---

    Code
      app$trigger(i)
    Condition
      Error in `app$trigger()`:
      ! "end" and other protected events cannot be triggered manually

---

    Code
      app$trigger(i)
    Condition
      Error in `app$trigger()`:
      ! "cycle-start" and other protected events cannot be triggered manually

---

    Code
      app$trigger(i)
    Condition
      Error in `app$trigger()`:
      ! "cycle-end" and other protected events cannot be triggered manually

---

    Code
      app$trigger(i)
    Condition
      Error in `app$trigger()`:
      ! "header" and other protected events cannot be triggered manually

---

    Code
      app$trigger(i)
    Condition
      Error in `app$trigger()`:
      ! "before-request" and other protected events cannot be triggered manually

---

    Code
      app$trigger(i)
    Condition
      Error in `app$trigger()`:
      ! "request" and other protected events cannot be triggered manually

---

    Code
      app$trigger(i)
    Condition
      Error in `app$trigger()`:
      ! "after-request" and other protected events cannot be triggered manually

---

    Code
      app$trigger(i)
    Condition
      Error in `app$trigger()`:
      ! "before-message" and other protected events cannot be triggered manually

---

    Code
      app$trigger(i)
    Condition
      Error in `app$trigger()`:
      ! "message" and other protected events cannot be triggered manually

---

    Code
      app$trigger(i)
    Condition
      Error in `app$trigger()`:
      ! "after-message" and other protected events cannot be triggered manually

---

    Code
      app$trigger(i)
    Condition
      Error in `app$trigger()`:
      ! "websocket-closed" and other protected events cannot be triggered manually

---

    Code
      app$trigger(i)
    Condition
      Error in `app$trigger()`:
      ! "send" and other protected events cannot be triggered manually

# data can be set, get and removed

    Code
      app$get_data(1)
    Condition
      Error in `app$get_data()`:
      ! `name` must be a single string, not the number 1.

---

    Code
      app$get_data(c("test", "test2"))
    Condition
      Error in `app$get_data()`:
      ! `name` must be a single string, not a character vector.

# plugins are being attached

    Code
      app$attach(plugin, 15)
    Condition
      Error in `app$attach()`:
      ! `plugin$name` must be a single string, not `NULL`.

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
    Message
      error: Testing an error from private$handleEnv[[id]](...)
    Code
      later::run_now()

---

    Code
      app$reignite(silent = TRUE, block = FALSE)
    Message
      error: Testing an error from private$handleEnv[[id]](...)
    Code
      later::run_now()

# futures can be added and called

    Code
      app$ignite(silent = TRUE)
    Message
      message: 10 from message(res)

---

    Code
      app$ignite(silent = TRUE)
    Message
      message: 10 from message(res)

---

    Code
      app$ignite()
    Message
      Fire started at <127.0.0.1:49925>
      message: 10 from message(res)

# ignite is blocked during run

    Code
      app$ignite()
      later::run_now()
    Message
      warning: Server is already running and cannot be started

# external triggers are fired

    Code
      app$ignite(silent = TRUE, block = FALSE)
    Message
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
      app$send("keep testing", client_to_id(req))
    Message
      keep testing

---

    Code
      app$send("keep testing again")
    Message
      keep testing again

---

    Code
      app$close_ws_con(client_to_id(req))
    Message
      closing

# showcase opens a browser

    Code
      app$ignite(showcase = TRUE)
    Message
      Fire started at <127.0.0.1:49925>
      Open browser

---

    Code
      app$ignite(showcase = TRUE, block = FALSE)
    Message
      Fire started at <127.0.0.1:49925>
      Open browser

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
    Message
      error: URL (/testing) not matching mount point (/test)

---

    Code
      res <- app$test_header(req)
    Message
      error: URL (/testing) not matching mount point (/test)

---

    Code
      app$test_websocket(req, "test")
    Message
      error: URL (/testing) not matching mount point (/test)
      closing

# Logging can be configured

    Code
      res <- app$trigger("test")
    Output
      test: this is a test

---

    Code
      app$test_request(fake_request("www.example.com/path", REMOTE_ADDR = "test"))
    Output
      request: test - ID_test [29/Jan/2025:08:17:44 +0100] "GET /path HTTP/1.1" 404 0
      $status
      [1] 404
      
      $headers
      $headers$`Content-Type`
      [1] "text/plain"
      
      
      $body
      [1] ""
      

# is_running works

    Code
      app$ignite(silent = TRUE)
    Message
      message: TRUE

# safe_call catches conditions

    Code
      private$safe_call(stop("error test"))
    Message
      message: error: error test from doTryCatch(return(expr), name, parentenv, handler) from NULL

---

    Code
      private$safe_call(warning("warning test"))
    Message
      message: warning: warning test from doTryCatch(return(expr), name, parentenv, handler) from NULL

---

    Code
      private$safe_call(message("message test"))
    Message
      message: message test from message("message test")

