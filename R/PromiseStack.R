#' @include aaa.R
NULL

#' @importFrom R6 R6Class
#'
DelayStack <- R6Class(
  'DelayStack',
  public = list(
    # Methods
    initialize = function(server) {
      private$server <- server
    },
    add = function(expr, then, ...) {
      expr <- enquo(expr)
      id <- reqres::random_key()
      private$calls[[id]] <- private$make_promise(expr, then, ...)
      invisible(id)
    },
    remove = function(id) {
      private$clear(id)
    },
    empty = function() {
      length(private$calls) == 0
    },
    eval = function(...) {
      if (!self$empty()) {
        evalIds <- private$do_eval()
        for (i in evalIds) {
          res <- private$server$safe_call(eval_tidy(private$calls[[i]]$expr))
          private$server$safe_call(private$calls[[i]]$then(res = res, ...))
          private$calls[[i]]$evaled <- TRUE
        }
        private$clear(evalIds)
      }
    }
  ),
  private = list(
    # Data
    calls = list(),
    server = NULL,

    # Methods
    make_promise = function(expr, then, ...) {
      if (missing(then)) {
        then <- private$null_fun
      } else {
        check_function(then)
      }
      list(
        expr = expr,
        then = then,
        evaled = FALSE,
        ...
      )
    },
    do_eval = function() {
      names(private$calls)
    },
    clear = function(ids, ...) {
      if (length(ids) > 0) {
        private$calls[ids] <- NULL
      }
    },
    null_fun = function(...) {
      NULL
    }
  )
)

#' @importFrom R6 R6Class
#'
TimeStack <- R6Class(
  'TimeStack',
  inherit = DelayStack,
  public = list(
    remove = function(id) {
      private$clear(id, force = TRUE)
    },
    reset = function() {
      for (id in names(private$calls)) {
        private$calls[[id]]$at <- Sys.time() + private$calls[[id]]$after
      }
    }
  ),
  private = list(
    make_promise = function(expr, then, after, loop = FALSE) {
      check_number_decimal(after)
      check_bool(loop)
      super$make_promise(
        expr = expr,
        then = then,
        after = after,
        loop = loop,
        at = Sys.time() + after
      )
    },
    do_eval = function() {
      names(private$calls)[
        vapply(private$calls, function(x) x$at, numeric(1)) < Sys.time()
      ]
    },
    clear = function(ids, force = FALSE) {
      if (!force) {
        remove <- vapply(
          ids,
          function(id) {
            if (private$calls[[id]]$loop && private$calls[[id]]$evaled) {
              private$calls[[id]]$at <- private$calls[[id]]$at +
                private$calls[[id]]$after
              FALSE
            } else {
              TRUE
            }
          },
          logical(1)
        )
        ids <- ids[remove]
      }
      super$clear(ids)
    }
  )
)

#' @importFrom R6 R6Class
#'
AsyncStack <- R6Class(
  'AsyncStack',
  inherit = DelayStack,
  public = list(
    # Methods
    add = function(expr, then, ...) {
      check_installed("future")
      id <- reqres::random_key()
      private$calls[[id]] <- private$make_promise(expr, then, ...)
      invisible(id)
    },
    eval = function(...) {
      if (!self$empty()) {
        evalIds <- private$do_eval()
        for (i in evalIds) {
          res <- private$server$safe_call(future::value(
            private$calls[[i]]$expr
          ))
          private$server$safe_call(private$calls[[i]]$then(res = res, ...))
        }
        if (length(evalIds) != 0) private$clear(evalIds)
      }
    }
  ),
  private = list(
    # Methods
    make_promise = function(expr, then, ...) {
      if (missing(then)) {
        then <- private$null_fun
      } else {
        check_function(then)
      }
      list(
        expr = eval_bare(call2(future::future, expr = expr, lazy = FALSE)),
        then = then,
        ...
      )
    },
    do_eval = function() {
      names(private$calls)[vapply(
        private$calls,
        function(x) future::resolved(x$expr, timeout = 0.05),
        logical(1)
      )]
    }
  )
)
