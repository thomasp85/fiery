#' App Logging
#' 
#' `fiery` has a build in logging mechanism that lets you capture event 
#' information however you like. Every user-injested warnings and errors are
#' automatically captured by the logger along with most system errors as well.
#' `fiery` tries very hard not to break due to faulty app logic. This means that
#' any event handler error will be converted to an error log without `fiery` 
#' stopping. In the case of request handlers a 500L response will be send back
#' if any error is encountered.
#' 
#' @section Setting a logger:
#' By default, `fiery` uses `logger_null()` which forwards warning and error 
#' messages to `stderr()` and ignores any other logging events. To change this 
#' behavior, set a different logger using the `set_logger()` method:
#' 
#' ```
#' app$set_logger(logger)
#' ```
#' 
#' where `logger` is a function taking at least the following arguments: `event`,
#' `message`, `request`, `time`, and `...`.
#' 
#' `fiery` comes with some additional loggers, which either writes all logs to a 
#' file or to the console. A new instance of the file logger can be created with 
#' `logger_file(file)`:
#' 
#' ```
#' app$set_logger(logger_file('fiery_log.log'))
#' ```
#' 
#' A new instance of the console logger can be create with `logger_console()`:
#' 
#' ```
#' app$set_logger(logger_console())
#' ```
#' 
#' Both functions takes a `format` a argument that lets you customise how the
#' log is written. Furthermore the console logger will style the logs with 
#' colour coding depending on the content if the console supports it.
#' 
#' As a last possibility it is possible to use different loggers dependent on 
#' the event by using the switch logger:
#' 
#' ```
#' app$set_logger(logger_switch(warning =, 
#'                              error = logger_file('errors.log),
#'                              default = logger_file('info.log')))
#' ```
#' 
#' @section Automatic logs:
#' `fiery` logs a number of different information by itself describing its 
#' operations during run. The following events are send to the log:
#' 
#' \describe{
#'  \item{*start*}{Will be send when the server starts up}
#'  \item{*resume*}{Will be send when the server is resumed}
#'  \item{*stop*}{Will be send when the server stops}
#'  \item{*request*}{Will be send when a request has been handled. The message
#'  will contain information about how long time it took to handle the request 
#'  or if it was denied.}
#'  \item{*websocket*}{Will be send every time a WebSocket connection is 
#'  established or closed as well as when a message is received or send}
#'  \item{*message*}{Will be send every time a message is emitted by an event 
#'  handler or delayed execution handler}
#'  \item{*warning*}{Will be send everytime a warning is emitted by an event 
#'  handler or delayed execution handler}
#'  \item{*error*}{Will be send everytime an error is signaled by an event 
#'  handler or delayed execution handler. In addition some internal functions
#'  will also emit error event when exceptions are encountered}
#' }
#' 
#' By default only *message*, *warning* and *error* events will be logged by 
#' sending them to the error stream as a [message()].
#' 
#' @section Access Logs:
#' Of particular interest are logs that detail requests made to the server. 
#' These are the `request` events detailed above. There are different standards
#' for how requests are logged. `fiery` uses the *Common Log Format* by default,
#' but this can be modified by setting the `access_log_format` field to a 
#' [glue][glue::glue] expression that has access to the following variables:
#' 
#' \describe{
#'  \item{`start_time`}{The time the request was recieved}
#'  \item{`end_time`}{The time the response was send back}
#'  \item{`request`}{The `Request` object}
#'  \item{`response`}{The `Response` object}
#'  \item{`id`}{The client id}
#' }
#' 
#' To change the format:
#' 
#' ```
#' app$access_log_format <- combined_log_format
#' ```
#' 
#' @section Custom logs:
#' Apart from the standard logs described above it is also possible to send 
#' messages to the log as you please, e.g. inside event handlers. This is done
#' through the `log()` method where you at the very least specify an event and a
#' message. In general it is better to send messages through `log()` rather than
#' with `warning()` and `stop()` even though the latters will eventually be
#' caught, as it gives you more control over the logging and what should happen
#' in the case of an exception.
#' 
#' An example of using `log()` in a handler could be:
#' 
#' ```
#' app$on('header', function(server, id, request) {
#'   server$log('info', paste0('request from ', id, ' received'), request)
#' })
#' ```
#' 
#' Which would log the timepoint the headers of a request has been recieved.
#' 
#' @rdname loggers
#' @name loggers
#' @aliases logging
#' 
NULL

#' @rdname loggers
#' @export
logger_null <- function() {
  function(event, message, request = NULL, time = Sys.time(), ...) {
    if (event %in% c('error', 'warning', 'message')) {
      message(event, ': ', trimws(message), sep = '')
    }
  }
}
#' @rdname loggers
#' 
#' @importFrom crayon red yellow blue green cyan magenta bold make_style
#' @export
logger_console <- function(format = '{time} - {event}: {message}') {
  orange <- make_style('orange')
  function(event, message, request = NULL, time = Sys.time(), ...) {
    msg <- glue_log(list(
      time = time,
      event = event,
      message = trimws(message)
    ), format)
    msg <- switch(event, error = red(msg), warning = yellow(msg), message = blue(msg), msg)
    if (event == 'request') {
      status_group <- as.integer(cut(request$respond()$status, breaks = c(100, 200, 300, 400, 500, 600), right = FALSE))
      msg <- switch(
        status_group,
        blue$bold(msg),
        green$bold(msg),
        cyan$bold(msg),
        orange$bold(msg),
        red$bold(msg)
      )
    }
    cat(msg, file = stdout(), append = TRUE)
    cat('\n', file = stdout(), append = TRUE)
  }
}
#' @rdname loggers
#' 
#' @param file A file or connection to write to
#' @param format A [glue][glue::glue] string specifying the format of the log entry
#' 
#' @export
logger_file <- function(file, format = '{time} - {event}: {message}') {
  format <- sub('\n$', '', format)
  function(event, message, request = NULL, time = Sys.time(), ...) {
    msg <- glue_log(list(
      time = time,
      event = event,
      message = trimws(message)
    ), format)
    cat(msg, file = file, append = TRUE)
    cat('\n', file = file, append = TRUE)
  }
}
#' @rdname loggers
#' 
#' @param ... A named list of loggers to use for different events. The same 
#' semantics as [switch][base::switch] is used so it is possible to let events
#' *fall through* e.g. `logger_switch(error =, warning = logger_file('errors.log'))`.
#' 
#' @param default A catch-all logger for use with events not defined in `...`
#' 
#' @export
logger_switch <- function(..., default = logger_null()) {
  enclos <- parent.frame()
  args <- eval(substitute(alist(...)))
  args <- lapply(args, function(e) {
    if (!identical(e, quote(expr = ))) {
      eval(e, envir = enclos)
    } else {
      e
    }
  })
  args <- c(args, list(default))
  function(event, message, request = NULL, time = Sys.time(), ...) {
    loc_args <- c(list(event), args)
    do.call(switch, loc_args)(event = event, message = message, request = request, time = time, ...)
  }
}
#' @rdname loggers
#' @export
common_log_format <- '{request$ip} - {id} [{format(end_time, "%d/%b/%Y:%T %z")}] "{toupper(request$method)} {request$path}{request$querystring} {toupper(request$protocol)}/1.1" {response$status} {response$content_length()}'
#' @rdname loggers
#' @export
combined_log_format <- paste0(common_log_format, ' "{request$get_header("Referer") %||% ""}" "{paste(request$get_header("User-Agent"), collapse = ", ") %||% ""}"')


# Helpers -----------------------------------------------------------------

# safely_transformer <- function(otherwise = NA) {
#   function(code, envir) {
#     tryCatch(eval(code, envir),
#              error = function(e) if (is.language(otherwise)) eval(otherwise) else otherwise)
#   }
# }
#' @importFrom glue glue_data
glue_log <- function(.data, ..., .envir = parent.frame()) {
  glue_data(.data, ..., .envir = .envir)
  #glue_data(.data, ..., .transformer = safely_transformer(''), .envir = emptyenv())
}
