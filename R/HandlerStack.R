#' @include aaa.R
NULL

#' @importFrom R6 R6Class
HandlerStack <- R6Class('HandlerStack',
  public = list(
    # Methods
    initialize = function(server = NULL) {
      private$server <- server
    },
    add = function(handler, id, pos = NULL) {
      check_string(id)
      check_function(handler)
      if (!rlang::is_primitive(handler) && ".request" %in% fn_fmls_names(handler)) {
        cli::cli_abort("{.arg request} is a reserved argument name and cannot be used in event handlers")
      }
      if (is.null(pos)) {
        pos <- length(private$handlers)
      } else {
        check_number_whole(pos, min = 1)
        pos <- max(0, min(c(pos - 1, length(private$handlers))))
      }
      private$handlers <- append(private$handlers, set_names(list(handler), id), pos)
    },
    remove = function(id) {
      check_string(id)
      handler <- private$handlers[[id]]
      private$handlers[[id]] <- NULL
      invisible(handler)
    },
    dispatch = function(..., .request = NULL) {
      lapply(private$handlers, function(handler) {
        private$server$safe_call(handler(...), request = .request)
      })
    },
    length = function() {
      length(private$handlers)
    },
    contains = function(id) {
      check_character(id)
      !is.null(private$handlers[[id]])
    },
    position = function(id) {
      check_character(id)
      match(id, names(private$handlers))
    }
  ),
  private = list(
    # Data
    handlers = list(),
    server = NULL
  )
)
