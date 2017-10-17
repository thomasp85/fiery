#' @include aaa.R
NULL

#' @importFrom R6 R6Class
#' @importFrom uuid UUIDgenerate
#' @importFrom future future resolved value
#' @importFrom assertthat assert_that
#' 
FutureStack <- R6Class('FutureStack',
  public = list(
    # Methods
    initialize = function(server) {
      private$server <- server
      private$futures <- new.env(parent = emptyenv())
    },
    add = function(expr, then, ..., substituted = FALSE) {
      if (!substituted) {
        expr <- substitute(expr)
      }
      id <- UUIDgenerate()
      private$futures[[id]] <- private$make_future(expr, then, ...)
      private$ids <- append(private$ids, id)
      invisible(id)
    },
    remove = function(id) {
      private$clear(id)
    },
    empty = function() {
      length(private$ids) == 0
    },
    eval = function(...) {
      if (!self$empty()) {
        evalIds <- private$ids[sapply(private$ids, private$do_eval)]
        for (i in evalIds) {
          res <- tri(value(private$futures[[i]]$expr))
          if (is.error_cond(res)) {
            private$server$log('error', conditionMessage(res))
          }
          res <- tri(do.call(private$futures[[i]]$then, list(res = res, ...)))
          if (is.error_cond(res)) {
            private$server$log('error', conditionMessage(res))
          }
        }
        if (length(evalIds) != 0) private$clear(evalIds)
      }
    }
  ),
  private = list(
    # Data
    ids = character(),
    futures = NULL,
    catcher = 'future',
    lazy = FALSE,
    server = NULL,
    
    # Methods
    make_future = function(expr, then, ...) {
      if (missing(then)) {
        then <- private$null_fun
      } else {
        assert_that(is.function(then))
      }
      list(
        expr = do.call(private$catcher,
                       list(expr = expr, lazy = private$lazy)),
        then = then,
        ...
      )
    },
    do_eval = function(id) {
      resolved(private$futures[[id]]$expr, timeout = 0.05)
    },
    clear = function(ids, ...) {
      if (length(ids) > 0) {
        private$ids <- private$ids[private$ids != ids]
        rm(list = ids, envir = private$futures)
      }
    },
    null_fun = function(...) {
      NULL
    }
  )
)

#' @importFrom R6 R6Class
#' @importFrom future sequential
#' 
DelayStack <- R6Class('DelayStack',
  inherit = FutureStack,
  private = list(
    catcher = 'sequential',
    lazy = TRUE
  )
)

#' @importFrom R6 R6Class
#' @importFrom future multiprocess
#' 
AsyncStack <- R6Class('AsyncStack',
  inherit = FutureStack,
  private = list(
    catcher = 'multiprocess'
  )
)

#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that is.number is.flag
#' 
TimeStack <- R6Class('TimeStack',
  inherit = DelayStack,
  public = list(
    remove = function(id) {
      private$clear(id, force = TRUE)
    },
    reset = function() {
      for (id in private$ids) {
        private$futures[[id]]$at <- Sys.time() + private$futures[[id]]$after
      }
    }
  ),
  private = list(
    make_future = function(expr, then, after, loop = FALSE) {
      assert_that(
        is.number(after),
        is.flag(loop)
      )
      super$make_future(expr = expr, then = then, after = after, 
                        loop = loop, at = Sys.time() + after)
    },
    do_eval = function(id) {
      Sys.time() > private$futures[[id]]$at
    },
    clear = function(ids, force = FALSE) {
      if (!force) {
        remove <- sapply(ids, function(id) {
          if (private$futures[[id]]$loop) {
            private$restore(private$futures[[id]]$expr)
            private$futures[[id]]$at <- private$futures[[id]]$at + 
              private$futures[[id]]$after
            FALSE
          } else {
            TRUE
          }
        })
        ids <- ids[remove]
      }
      super$clear(ids)
    },
    restore = function(future) {
      if (future$state == 'finished') {
        future$state <- 'created'
        rm('value', envir = future)
      }
    }
  )
)
