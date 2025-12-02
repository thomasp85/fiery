#' @include aaa.R
NULL

HandlerStack <- function(server = safe_call_server()) {
  SAFE_CALL <- environment(server$clone)$private$p_safe_call
  HANDLERS <- list()

  obj <- structure(list(), class = "HandlerStack")

  obj$add <- function(handler, id, pos = NULL) {
    check_string(id)
    check_function(handler)
    if (
      !rlang::is_primitive(handler) && ".request" %in% fn_fmls_names(handler)
    ) {
      cli::cli_abort(
        "{.arg request} is a reserved argument name and cannot be used in event handlers"
      )
    }
    if (is.null(pos)) {
      pos <- length(HANDLERS)
    } else {
      check_number_whole(pos, min = 1)
      pos <- max(0, min(c(pos - 1, length(HANDLERS))))
    }
    HANDLERS <<- append(
      HANDLERS,
      set_names(list(handler), id),
      pos
    )
  }
  obj$remove <- function(id) {
    check_string(id)
    handler <- HANDLERS[[id]]
    HANDLERS[[id]] <<- NULL
    invisible(handler)
  }
  obj$dispatch <- function(..., .request = NULL) {
    lapply(HANDLERS, function(h) SAFE_CALL(h(...), request = .request))
  }
  obj$length <- function() {
    length(HANDLERS)
  }
  obj$contains <- function(id) {
    check_character(id)
    !is.null(HANDLERS[[id]])
  }
  obj$position <- function(id) {
    check_character(id)
    match(id, names(HANDLERS))
  }
  obj
}
