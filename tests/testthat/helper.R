standard_app <- function(silent = TRUE) {
  withr::with_seed(1, app <- Fire$new(port = 49925))
  logger <- logger_console("{event}: {message}")
  if (!silent) {
    app$set_logger(logger)
  } else {
    app$set_logger(function(event, message, request, ...) {
      if (event %in% c("message", "error", "warning")) {
        logger(event, message, request, ...)
      }
    })
  }
  app$access_log_format <- '{request$ip} - {id} [29/Jan/2025:08:17:44 +0100] "{toupper(request$method)} {request$path}{request$querystring} {toupper(request$protocol)}/1.1" {response$status} {response$content_length()}'
  app
}

r_session <- function(silent = TRUE) {
  skip_on_cran() # All these tests are too fickle to test on CRAN
  rs <- callr::r_session$new()
  caller <- function(expr, invisible = TRUE) {
    expr <- enexpr(expr)
    val <- rs$run_with_output(
      function(x) rlang::eval_bare(expr = x, globalenv()),
      args = list(expr)
    )
    if (val$stdout != "") {
      cat(val$stdout)
    }
    if (val$stderr != "") {
      rlang::inform(trimws(val$stderr))
    }
    if (!is.null(val$error)) {
      cnd_signal(val$error)
    }
    if (invisible) {
      invisible(val$result)
    } else {
      val$result
    }
  }

  caller(library(fiery))
  caller(library(rlang))
  caller(options(rlang_backtrace_on_error = "none"))
  caller({
    standard_app <- function() {
      app <- Fire$new(port = 49925)
      logger <- logger_console("{event}: {message}")
      if (!!silent) {
        app$set_logger(function(event, message, request, ...) {
          if (event %in% c("message", "error", "warning")) {
            logger(event, message, request, ...)
          }
        })
      } else {
        app$set_logger(logger)
      }
      app$access_log_format <- '{request$ip} - {id} [29/Jan/2025:08:17:44 +0100] "{toupper(request$method)} {request$path}{request$querystring} {toupper(request$protocol)}/1.1" {response$status} {response$content_length()}'
      app
    }
  })
  caller(app <- standard_app())

  structure(caller, session = rs)
}
