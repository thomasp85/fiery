#' Event Overview
#' 
#' fiery is using an event-based model to allow you to program the logic. During
#' the life cycle of an app a range of different events will be triggered and it
#' is possible to add event handlers to these using the `on()` method. An event 
#' handler is simply a function that will get called every time an event is 
#' fired. Apart from the predefined life cycle events it is also possible to 
#' trigger custom events using the `trigger()` method. Manual triggering of life
#' cycle events is not allowed.
#' 
#' @section Life cycle Events:
#' 
#' Following is a list of all life cycle events. These cannot be triggered
#' manually, but is fired as part of the normal lifetime of a `fiery` server:
#' 
#' \describe{
#'  \item{start}{Will trigger once when the app is started but before it is 
#'  running. The handlers will receive the app itself as the `server` argument
#'  as well as any argument passed on from the `ignite()` method. Any return
#'  value is discarded.}
#'  \item{resume}{Will trigger once after the start event if the app has been 
#'  started using the `reignite()` method. The handlers will receive the app
#'  itself as the `server` argument as well as any argument passed on from the
#'  `reignite()` method. Any return value is discarded.}
#'  \item{end}{Will trigger once after the app is stopped. The handlers will 
#'  receive the app itself as the `server` argument. Any return value is 
#'  discarded.}
#'  \item{cycle-start}{Will trigger in the beginning of each loop, before the 
#'  request queue is flushed. The handlers will receive the app itself as the 
#'  `server` argument. Any return value is discarded.}
#'  \item{cycle-end}{Will trigger in the end of each loop, after the request
#'  queue is flushed and all delayed, timed, and asynchronous calls have been
#'  executed. The handlers will receive the app itself as the `server` argument.
#'  Any return value is discarded.}
#'  \item{header}{Will trigger every time the header of a request is received. 
#'  The return value of the last called handler is used to determine if further 
#'  processing of the request will be done. If the return value is `TRUE` the
#'  request will continue on to normal processing. If the return value is 
#'  `FALSE` the response will be send back and the connection will be closed 
#'  without retrieving the payload. The handlers will receive the app itself as 
#'  the `server` argument, the client id as the `id` argument and the request
#'  object as the `request` argument}
#'  \item{before-request}{Will trigger prior to handling of a request (that is, 
#'  every time a request is received unless it is short-circuited by the
#'  `header` handlers). The return values of the handlers will be passed on to
#'  the request handlers and can thus be used to inject data into the request 
#'  handlers (e.g. session specific data). The handlers will receive the app 
#'  itself as the `server` argument, the client id as the `id` argument and the 
#'  request object as the `request` argument}
#'  \item{request}{Will trigger after the `before-request` event. This is where 
#'  the main request handling is done. The return value of the last handler is 
#'  send back to the client as response. If no handler is registered a `404`
#'  error is returned automatically. If the return value is not a valid
#'  response, a `500` server error is returned instead. The handlers will
#'  receive the app itself as the `server` argument, the client id as the `id` 
#'  argument, the request object as the `request` argument, and the list of 
#'  values created by the before-event handlers as the `arg_list` argument.}
#'  \item{after-request}{Will trigger after the `request` event. This can be
#'  used to inspect the response (but not modify it) before it is send to the
#'  client. The handlers will receive the app itself as the `server` argument,
#'  the client id as the `id` argument, the request object as the `request`
#'  argument, and the response as the `response` argument. Any return value is
#'  discarded.}
#'  \item{before-message}{This event is triggered when a websocket message is 
#'  received. As with the `before-request` event the return values of the
#'  handlers are passed on to the `message` handlers. Specifically if a
#'  `'binary'` and `'message'` value is returned they will override the original
#'  values in the `message` and `after-message` handler arguments. This can e.g.
#'  be used to decode the message once before passing it through the `message`
#'  handlers. The `before-message` handlers will receive the app itself as the
#'  `server` argument, the client id as the `id` argument, a flag indicating
#'  whether the message is binary as the `binary` argument, the message itself
#'  as the `message` argument, and the request object used to establish the 
#'  connection with the client as the `request` argument.}
#'  \item{message}{This event is triggered after the `before-message` event and
#'  is used for the primary websocket message handling. As with the `request`
#'  event, the handlers for the `message` event receives the return values from
#'  the `before-message` handlers which can be used to e.g. inject session
#'  specific data. The message handlers will receive the app itself as the
#'  `server` argument, the client id as the `id` argument, a flag indicating
#'  whether the message is binary as the `binary` argument, the message itself
#'  as the `message` argument, the request object used to establish the 
#'  connection with the client as the `request` argument, and the values 
#'  returned by the before-message handlers as the `arg_list` argument. Contrary
#'  to the `request` event the return values of the handlers are ignored as
#'  websocket communication is bidirectional}
#'  \item{after-message}{This event is triggered after the `message` event. It
#'  is provided more as an equivalent to the `after-request` event than out of 
#'  necessity as there is no final response to inspect and handler can thus just
#'  as well be attached to the `message` event. For clear division of server
#'  logic, message specific handlers should be attached to the `message` event,
#'  whereas general handlers should, if possible, be attached to the
#'  `after-message` event. The `after-message` handlers will receive the app
#'  itself as the `server` argument, the client id as the `id` argument, a flag 
#'  indicating whether the message is binary as the `binary` argument, the 
#'  message itself as the `message` argument, and the request object used to
#'  establish the connection with the client as the `request` argument.}
#'  \item{send}{This event is triggered after a websocket message is send to a 
#'  client. The handlers will receive the app itself as the `server` argument,
#'  the client id as the `id` argument and the send message as the `message`
#'  argument. Any return value is discarded.}
#'  \item{websocket-closed}{This event will be triggered every time a websocket 
#'  connection is closed. The handlers will receive the app itself as the 
#'  `server` argument, the client id as the `id` argument and request used to
#'  establish the closed connection as the `request` argument. Any return value
#'  is discarded.}
#' }
#' 
#' @section Custom Events:
#' Apart from the predefined events, it is also possible to trigger and listen
#' to custom events. The syntax is as follows:
#' 
#' ```
#' # Add a handler to the 'new-event' event
#' id <- app$on('new-event', function() {
#'   message('Event fired')
#' })
#' 
#' # Trigger the event
#' app$trigger('new-event')
#' 
#' # Remove the handler
#' app$off(id)
#' ```
#' 
#' Additional parameters passed on to the `trigger()` method will be passed on
#' to the handler. There is no limit to the number of handlers that can be 
#' attached to custom events. When an event is triggered they will simply be 
#' called in the order they have been added. Triggering a non-existing event is 
#' not an error, so plugins are free to fire off events without worrying about 
#' whether handlers have been added.
#' 
#' @section Triggering Events Externally:
#' If a `fiery` server is running in blocking mode it is not possible to 
#' communicate with it using the `trigger()` method. Instead it is possible to
#' assign a directory to look in for event trigger instructions. The trigger 
#' directory is set using the `trigger_dir` field, e.g.:
#' 
#' ```
#' app$trigger_dir <- '/some/path/to/dir/'
#' ```
#' 
#' Events are triggered by placing an `rds` file named after the event in the
#' trigger directory. The file must contain a list, and the elements of the list
#' will be passed on as arguments to the event handlers. After the event has
#' been triggered the file will be deleted. The following command will trigger
#' the `external-event` on a server looking in `'/some/path/to/dir/'`:
#' 
#' ```
#' saveRDS(list(arg1 = 'test'), '/some/path/to/dir/external-event.rds')
#' ```
#' 
#' @seealso [`Fire`] describes how to create a new server
#' 
#' [plugins] describes how to use plugins to modify the server
#' 
#' @rdname event_doc
#' @name event_doc
#' @aliases events
#' 
NULL
