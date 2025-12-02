#' @include aaa.R
NULL

DelayStack <- function(server = safe_call_server()) {
  SAFE_CALL <- environment(server$clone)$private$p_safe_call
  CALLS <- list()

  make_promise <- function(expr, then, ...) {
    if (missing(then)) {
      then <- function(...) NULL
    } else {
      check_function(then)
    }
    list(
      expr = expr,
      then = then,
      evaled = FALSE,
      ...
    )
  }
  clear <- function(ids, ...) {
    if (length(ids) > 0) {
      CALLS[ids] <<- NULL
    }
  }
  empty <- function() {
    length(CALLS) == 0
  }

  obj <- structure(list(), class = "DelayStack")

  obj$add <- function(expr, then, ...) {
    expr <- enquo(expr)
    id <- reqres::random_key()
    CALLS[[id]] <<- make_promise(expr, then, ...)
    invisible(id)
  }
  obj$remove <- clear
  obj$empty <- empty
  obj$eval <- function(...) {
    for (i in names(CALLS)) {
      SAFE_CALL({
        res <- eval_tidy(CALLS[[i]]$expr)
        CALLS[[i]]$then(res = res, ...)
      })
    }
    CALLS <<- list()
  }

  obj
}

TimeStack <- function(server = safe_call_server()) {
  SAFE_CALL <- environment(server$clone)$private$p_safe_call
  CALLS <- list()

  make_promise <- function(expr, then, after, loop = FALSE) {
    check_number_decimal(after)
    check_bool(loop)
    if (missing(then)) {
      then <- function(...) NULL
    } else {
      check_function(then)
    }
    list(
      expr = expr,
      then = then,
      evaled = FALSE,
      after = after,
      loop = loop,
      at = Sys.time() + after
    )
  }
  clear <- function(ids, force = FALSE) {
    if (!force) {
      remove <- vapply(
        ids,
        function(id) {
          if (CALLS[[id]]$loop && CALLS[[id]]$evaled) {
            CALLS[[id]]$at <<- CALLS[[id]]$at +
              CALLS[[id]]$after
            FALSE
          } else {
            TRUE
          }
        },
        logical(1)
      )
      ids <- ids[remove]
    }
    if (length(ids) > 0) {
      CALLS[ids] <<- NULL
    }
  }
  do_eval <- function() {
    names(CALLS)[
      vapply(CALLS, function(x) x$at, numeric(1)) < Sys.time()
    ]
  }
  empty <- function() {
    length(CALLS) == 0
  }

  obj <- structure(list(), class = "TimeStack")

  obj$add <- function(expr, then, ...) {
    expr <- enquo(expr)
    id <- reqres::random_key()
    CALLS[[id]] <<- make_promise(expr, then, ...)
    invisible(id)
  }
  obj$remove <- function(id) clear(id, force = TRUE)
  obj$empty <- empty
  obj$reset <- function() {
    for (id in names(CALLS)) {
      CALLS[[id]]$at <<- Sys.time() + CALLS[[id]]$after
    }
  }
  obj$eval <- function(...) {
    if (length(CALLS) != 0) {
      eval_ids <- do_eval()
      for (i in eval_ids) {
        SAFE_CALL({
          res <- eval_tidy(CALLS[[i]]$expr)
          CALLS[[i]]$then(res = res, ...)
          CALLS[[i]]$evaled <<- TRUE
        })
      }
      clear(eval_ids)
    }
  }

  obj
}

#' @importFrom R6 R6Class
#'
AsyncStack <- R6Class(
  'AsyncStack',
  public = list(
    initialize = function(server) {
      private$server <- server
    },
    remove = function(id) {
      private$clear(id)
    },
    empty = function() {
      length(private$calls) == 0
    },
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
