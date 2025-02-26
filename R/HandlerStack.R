#' @include aaa.R
NULL

#' @importFrom R6 R6Class
HandlerStack <- R6Class('HandlerStack',
  public = list(
    # Methods
    initialize = function(server = NULL) {
      private$handleEnv <- new.env(parent = emptyenv())
      private$server <- server
    },
    add = function(handler, id, pos = NULL) {
      check_string(id)
      check_function(handler)
      if (!rlang::is_primitive(handler) && ".request" %in% fn_fmls_names(handler)) {
        cli::cli_abort("{.arg request} is a reserved argument name and cannot be used in event handlers")
      }
      if (is.null(pos)) {
        pos <- length(private$handleOrder) + 1
      } else {
        check_number_whole(pos, min = 1)
        pos <- min(c(pos, length(private$handleOrder) + 1))
      }
      assign(id, handler, envir = private$handleEnv)
      private$handleOrder <- append(private$handleOrder, id, after = pos - 1)
    },
    remove = function(id) {
      check_string(id)
      ind <- which(private$handleOrder == id)
      if (length(ind) != 0) {
        private$handleOrder <- private$handleOrder[-ind]
        handler <- private$handleEnv[[id]]
        rm(list = id, envir = private$handleEnv)
        invisible(handler)
      }
    },
    dispatch = function(..., .request = NULL) {
      res <- lapply(private$handleOrder, function(id) {
        private$server$safe_call(private$handleEnv[[id]](...), request = .request)
      })
      names(res) <- private$handleOrder
      res
    },
    length = function() {
      length(private$handleOrder)
    },
    contains = function(id) {
      check_character(id)
      !is.na(self$position(id))
    },
    position = function(id) {
      check_character(id)
      match(id, private$handleOrder)
    }
  ),
  private = list(
    # Data
    handleEnv = NULL,
    handleOrder = character(),
    server = NULL
  )
)
