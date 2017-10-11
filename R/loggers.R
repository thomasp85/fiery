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
#' By default, `fiery` uses `null_logger` which forwards warning and error 
#' messages to `stderr()` and ignores any other logging events. To change this 
#' behavior, set a different logger using the `set_logger()` method:
#' 
#' ```
#' app$set_logger(logger)
#' ```
#' 
#' where `logger` is a function taking at least the following arguments: `event`,
#' `message`, `request`, and `...`.
#' 
#' `fiery` comes with one additional logger, which writes all logs to a file. A
#' new instance of the file logger can be created with 
#' `create_file_logger(file)`:
#' 
#' ```
#' app$set_logger(create_file_logger('fiery_log.log'))
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
#'  established or closed.}
#'  \item{*message*}{Will be send every time a WebSocket message is received or
#'  send}
#'  \item{*warning*}{Will be send everytime a warning is emitted by an event 
#'  handler or delayed execution handler}
#'  \item{*error*}{Will be send everytime an error is signaled by an event 
#'  handler or delayed execution handler. In addition some internal functions
#'  will also emit error event when exceptions are encountered}
#' }
#' 
#' By default only *warning* and *error* events will be logged by sending them
#' to the error stream.
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
#' 
#' @param event A string giving the type of event
#' 
#' @param message A string describing the particular event in detail
#' 
#' @param request either `NULL` or a `Request` object associated with the log
#' 
#' @param ... Additional information passed on from the `log()` method
#' 
#' @export
null_logger <- function(event, message, request = NULL, ...) {
    if (event %in% c('error', 'warning')) {
        cat(event, ': ', message, file = stderr(), sep = '')
    }
}

#' @rdname loggers
#' 
#' @param file A file or connection to write to
#' 
#' @export
create_file_logger <- function(file) {
    function(event, message, request = NULL, ...) {
        msg <- paste0(Sys.time(), ' - ', event, ': ', message)
        if (event == 'request' && !is.null(request)) {
            msg <- paste0(msg, ' (', toupper(request$method),' ', request$url, 'returning ', request$respond()$status, ')')
        }
        cat(paste0(msg, '\n'), file = file, append = TRUE)
    }
}