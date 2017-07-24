#' @include aaa.R
#' @include HandlerStack.R
NULL

#' Generate a New App Object
#' 
#' The Fire generator creates a new 'Fire'-object, which is the the class 
#' containing all app logic. The class is based on the R6 oo-system and is thus
#' reference-based with methods and data attached to each object, in contrast to
#' the more well known S3 and S4 systems.
#' 
#' @usage NULL
#' @format NULL
#' 
#' @section Initialization:
#' A new 'Fire'-object is initialized using the \code{new()} method on the 
#' generator:
#' 
#' \strong{Usage}
#' \tabular{l}{
#'  \code{app <- Fire$new(host = '127.0.0.1', port = 8080L)}
#' }
#' 
#' \strong{Arguments}
#' \tabular{lll}{
#'  \code{host} \tab  \tab A string overriding the default host (see the Fields section below)\cr
#'  \code{port} \tab  \tab An integer overriding the default port (see the Fields section below)
#' }
#' 
#' \emph{Copying}
#' 
#' As 'Fire' objects are using reference semantics new copies of an app cannot
#' be made simply be assigning it to a new variable. If a true copy of a 'Fire'
#' object is desired, use the \code{clone()} method.
#' 
#' @section Fields:
#' \describe{
#'  \item{\code{host}}{A string giving a valid IPv4 address owned by the server, or '0.0.0.0' to listen on all addresses. The default is '127.0.0.1'}
#'  \item{\code{port}}{An integer giving the port number the server should listen on (defaults to 8080L)}
#'  \item{\code{refreshRate}}{The interval in seconds between run cycles when running a blocking server (defaults to 0.001)}
#'  \item{\code{refreshRateNB}}{The interval in seconds between run cycles when running a non-bocking server (defaults to 1)}
#'  \item{\code{triggerDir}}{A valid folder where trigger files can be put when running a blocking server (defaults to NULL)}
#'  \item{\code{plugins}}{A named list of the already attached plugins. Static - can only be modified using the \code{attach()} method.}
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{ignite(block = TRUE, showcase = FALSE, ...)}}{Begins the server,either blocking the console if \code{block = TRUE} or not. If \code{showcase = TRUE} a browser window is opened directing at the server address. \code{...} will be redirected to the 'start' handler(s)}
#'  \item{\code{start(block = TRUE, showcase = FALSE, ...)}}{A less dramatic synonym of for \code{ignite}}
#'  \item{\code{reignite(block = TRUE, showcase = FALSE, ...)}}{As \code{ignite} but additionally triggers the 'resume' event after the 'start' event}
#'  \item{\code{resume(block = TRUE, showcase = FALSE, ...)}}{Another less dramatic synonym, this time for reignite}
#'  \item{\code{extinguish()}}{Stops a running server}
#'  \item{\code{stop()}}{Boring synonym for \code{extinguish}}
#'  \item{\code{on(event, handler, pos = NULL)}}{Add a handler function to to an event at the given position in the handler stack. Returns a string uniquely identifying the handler}
#'  \item{\code{off(handlerId)}}{Remove the handler tied to the given id}
#'  \item{\code{trigger(event, ...)}}{Triggers an event passing the additional arguments to the potential handlers}
#'  \item{\code{send(message, id)}}{Sends a websocket message to the client with the given id, or to all connected clients if id is missing}
#'  \item{\code{attach(plugin, ..., force = FALSE)}}{Attaches a plugin to the server. A plugin is an R6 object with an \code{onAttach} method and a \code{name} and \code{require} field. Plugins can only get attached once unless \code{force = TRUE}}
#'  \item{\code{has_plugin(name)}}{Check whether a plugin with the given name has been attached}
#'  \item{\code{header(name, value)}}{Add a global header to the server that will be set on all responses}
#'  \item{\code{set_data(name, value)}}{Adds data to the servers internal data store}
#'  \item{\code{get_data(name)}}{Extracts data from the internal data store}
#'  \item{\code{remove_data(name)}}{Removes the data with the given name from the internal data store}
#'  \item{\code{time(expr, then, after, loop = FALSE)}}{Add a timed evaluation that will be evaluated after the given number of seconds, potentially repeating if loop=TRUE. After the expression has evaluated the 'then' function will get called with the result of the expression and the server object as arguments.}
#'  \item{\code{remove_time(id)}}{Removes the timed evaluation identified by the id (returned when adding the evaulation)}
#'  \item{\code{delay(expr, then)}}{As time except the expr is evaluated immediately at the end of the loop cycle}
#'  \item{\code{remove_delay(id)}}{Removes the delayed evaluation identified by the id}
#'  \item{\code{async(expr, then)}}{As delay and time except the expression is evaluated asynchronously. The progress of evaluation is checked at the end of each loop cycle}
#'  \item{\code{remove_async(id)}}{Removes the async evaluation identified by the id. The evaluation is not necessarily stopped but the then function will not get called.}
#'  \item{\code{set_client_id_converter(converter)}}{Sets the function that converts an HTTP request into a specific client id}
#'  \item{\code{clone()}}{Create a copy of the full 'Fire' object and return that}
#' }
#' 
#' @section Events:
#' fiery is using an event-based model to allow you to program the logic. During
#' the lifecycle of an app a range of different events will be triggered and it
#' is possible to add event handlers to these using the \code{on()} method. An 
#' event handler is simply a function that will get called every time an event 
#' is fired. Apart from the predefined lifecycle events it is also possible to 
#' trigger custom events using the \code{trigger()} method. Manual triggering of
#' lifecycle events is not allowed.
#' 
#' Following is a list of all lifecycle events:
#' 
#' \describe{
#'  \item{start}{Will trigger once when the app is started but before it is 
#'  running. The handlers will recieve the app itself as the \code{server} 
#'  argument as well as any argument passed on from the \code{ignite()} method. 
#'  Any return value is discarded.}
#'  \item{resume}{Will trigger once after the start event if the app has been
#'  started using the \code{reignite()} method. The handlers will recieve the 
#'  app itself as the \code{server} argument as well as any argument passed on 
#'  from the \code{reignite()} method. Any return value is discarded.}
#'  \item{end}{Will trigger once after the app is stopped. The handlers will 
#'  recieve the app itself as the \code{server} argument. Any return value is 
#'  discarded.}
#'  \item{cycle-start}{Will trigger in the beginning of each loop, before the 
#'  request queue is flushed. The handlers will recieve the app itself as the 
#'  \code{server} argument. Any return value is discarded.}
#'  \item{cycle-end}{Will trigger in the end of each loop, after the 
#'  request queue is flushed and all delayed, timed, and asynchronous calls have
#'  been executed. The handlers will recieve the app itself as the \code{server} 
#'  argument. Any return value is discarded.}
#'  \item{header}{Will trigger everytime a the header of a request is recieved. 
#'  The return value of the last called handler is used to determine if further
#'  processing of the request will be done. If the return value is \code{NULL}
#'  the request will continue on to normal processing. If the return value is a
#'  response this will be send back and the connection will be closed without
#'  retrieving the payload. The handlers will recieve the app itself as the 
#'  \code{server} argument, the client id as the \code{id} argument and the 
#'  request object as the \code{request} argument}
#'  \item{before-request}{Will trigger prior to handling of a request (that is, 
#'  every time a request is recieved unless it is short-circuited by the header
#'  handlers). The return values of the handlers will be passed on to the request
#'  handlers and can thus be used to inject data into the request handlers (e.g.
#'  session specific data). The handlers will recieve the app itself as the 
#'  \code{server} argument, the client id as the \code{id} argument and the 
#'  request object as the \code{request} argument}
#'  \item{request}{Will trigger after the before-request event. This is where 
#'  the main request handling is done. The return value of the last handler is
#'  send back to the client as response. If no handler is reqistered a 404 error
#'  is returned automatically. If the return value is not a valid response,
#'  a 500 server error is returned instead. The handlers will recieve the app 
#'  itself as the \code{server} argument, the client id as the \code{id} 
#'  argument, the request object as the \code{request} argument, and the list of 
#'  values created by the before-event handlers as the \code{arg_list} argument.}
#'  \item{after-request}{Will trigger after the request event. This can be used
#'  to inspect the response (but not modify it) before it is send to the client. 
#'  The handlers will recieve the app itself as the \code{server} argument, the 
#'  client id as the \code{id} argument, the request object as the 
#'  \code{request} argument, and the response as the \code{response} argument. 
#'  Any return value is discarded.}
#'  \item{before-message}{This event is triggered when a websocket message is
#'  recieved. As with the before-request event the return values of the handlers
#'  are passed on to the message handlers. Specifically if a 'binary' and 
#'  'message' value is returned they will override the original values in the
#'  message and after-message handler arguments. This can e.g. be used to decode 
#'  the message once before passing it through the message handlers. The 
#'  before-message handlers will recieve the app itself as the \code{server} 
#'  argument, the client id as the \code{id} argument, a flag indicating whether 
#'  the message is binary as the \code{binary} argument, the message itself as 
#'  the \code{message} argument, and the request object used to establish the 
#'  connection with the client as the \code{request} argument.}
#'  \item{message}{This event is triggered after the before-message event and is
#'  used for the primary websocket message handling. As with the request event,
#'  the handlers for the message event recieves the return values from the 
#'  before-message handlers which can be used to e.g. inject session specific
#'  data. The message handlers will recieve the app itself as the \code{server} 
#'  argument, the client id as the \code{id} argument, a flag indicating whether 
#'  the message is binary as the \code{binary} argument, the message itself as 
#'  the \code{message} argument, the request object used to establish the 
#'  connection with the client as the \code{request} argument, and the values
#'  returned by the before-message handlers as the \code{arg_list} argument. 
#'  Contrary to the request event the return values of the handlers are ignored
#'  as websocket communication is bidirectional}
#'  \item{after-message}{This event is triggered after the message event. It is
#'  provided more as an equivalent to the after-request event than out of 
#'  necessity as there is no final response to inspect and handler can thus just
#'  as well be atached to the message event. For clear division of server logic 
#'  message specific handlers should be attached to the message event, whereas 
#'  general handlers should, if possible, be attached to the after-message 
#'  event. The after-message handlers will recieve the app itself as the 
#'  \code{server} argument, the client id as the \code{id} argument, a flag 
#'  indicating whether the message is binary as the \code{binary} argument, the 
#'  message itself as the \code{message} argument, and the request object used 
#'  to establish the connection with the client as the \code{request} argument.}
#'  \item{send}{This event is triggered after a websocket message is send to a 
#'  client. The handlers will recieve the app itself as the \code{server} 
#'  argument, the client id as the \code{id} argument and the send message as 
#'  the \code{message} argument. Any return value is discarded.}
#'  \item{websocket-closed}{This event will be triggered every time a websocket
#'  connection is closed. The handlers will recieve the app itself as the 
#'  \code{server} argument, the client id as the \code{id} argument and request
#'  used to establish the closed connection as the \code{request} argument. Any 
#'  return value is discarded.}
#' }
#' 
#' @importFrom R6 R6Class
#' @importFrom assertthat is.string is.count is.number has_args assert_that is.dir is.flag has_name is.error
#' @importFrom httpuv startServer service startDaemonizedServer stopDaemonizedServer stopServer
#' @importFrom uuid UUIDgenerate
#' @importFrom utils browseURL
#' @importFrom later later
#' @importFrom stats setNames
#' 
#' @export
#' @docType class
#' 
#' @examples 
#' # Create a New App
#' app <- Fire$new(port = 4689)
#' 
#' # Setup the data everytime it starts
#' app$on('start', function(server, ...) {
#'     server$set_data('visits', 0)
#'     server$set_data('cycles', 0)
#' })
#' 
#' # Count the number of cycles
#' app$on('cycle-start', function(server, ...) {
#'     server$set_data('cycles', server$get_data('cycles') + 1)
#' })
#' 
#' # Count the number of requests
#' app$on('before-request', function(server, ...) {
#'     server$set_data('visits', server$get_data('visits') + 1)
#' })
#' 
#' # Handle requests
#' app$on('request', function(server, ...) {
#'     list(
#'         status = 200L,
#'         headers = list('Content-Type' = 'text/html'),
#'         body = paste('This is indeed a test. You are number', server$get_data('visits'))
#'     )
#' })
#' 
#' # Show number of requests in the console
#' app$on('after-request', function(server, ...) {
#'     message(server$get_data('visits'))
#'     flush.console()
#' })
#' 
#' # Terminate the server after 300 cycles
#' app$on('cycle-end', function(server, ...) {
#'     if (server$get_data('cycles') > 300) {
#'         message('Ending...')
#'         flush.console()
#'         server$extinguish()
#'     }
#' })
#' 
#' # Be polite
#' app$on('end', function(server) {
#'     message('Goodbye')
#'     flush.console()
#' })
#' 
#' \dontrun{
#' app$ignite(showcase = TRUE)
#' }
#' 
Fire <- R6Class('Fire',
    public = list(
        # Methods
        initialize = function(host = '127.0.0.1', port = 8080) {
            self$host <- host
            self$port <- port
            private$data <- new.env(parent = emptyenv())
            private$handlers <- new.env(parent = emptyenv())
            private$websockets <- new.env(parent = emptyenv())
            private$client_id <- client_to_id
            private$DELAY <- DelayStack$new()
            private$TIME <- TimeStack$new()
            private$ASYNC <- AsyncStack$new()
        },
        print = function(...) {
            cat('\U0001f525 A fiery webserver\n')
            cat('\U0001f525  \U0001f4a5   \U0001f4a5   \U0001f4a5\n')
            plugins <- paste(names(private$pluginList), collapse = ', ')
            if (plugins == '') plugins <- 'none'
            cat('\U0001f525 Plugins attached: ', plugins, '\n', sep = '')
            handlers <- lapply(private$handlers, function(x) x$length())
            if (length(handlers) == 0) {
                cat('\U0001f525 Event handlers added: none\n')
            } else {
                cat('\U0001f525 Event handlers added\n')
                order <- match(names(handlers), private$privateTriggers)
                order[is.na(order)] <- seq_len(sum(is.na(order))) + max(order, na.rm = TRUE)
                handlers <- handlers[order(order)]
                name_length <- max(max(nchar(names(handlers))), 20)
                names(handlers) <- sprintf(paste0('%', name_length, 's'), names(handlers))
                for(i in names(handlers)) cat('\U0001f525 ', i, ': ', handlers[[i]], '\n', sep = '')
            }
        },
        ignite = function(block = TRUE, showcase = FALSE, ..., silent = FALSE) {
            if (!silent) message('Fire started at ', self$host, ':', self$port)
            private$run(block = block, showcase = showcase, ...)
            invisible(NULL)
        },
        start = function(block = TRUE, showcase = FALSE, ..., silent = FALSE) {
            self$ignite(block = block, showcase = showcase, ..., silent = silent)
        },
        reignite = function(block = TRUE, showcase = FALSE, ..., silent = FALSE) {
            if (!silent) message('Fire restarted at ', self$host, ':', self$port)
            private$run(block = block, resume = TRUE, showcase = showcase, ...)
            invisible(NULL)
        },
        resume = function(block = TRUE, showcase = FALSE, ..., silent = FALSE) {
            self$reignite(block = block, showcase = showcase, ..., silent = silent)
        },
        extinguish = function() {
            if (private$running) {
                if (!is.null(private$server)) {
                    if (private$nb_cycle) {
                        message('Cannot stop server from within a non-blocking event cycle')
                        flush.console()
                    } else {
                        private$running <- FALSE
                        private$p_trigger('end', server = self)
                        stopDaemonizedServer(private$server)
                        private$server <- NULL
                    }
                } else {
                    private$quitting <- TRUE
                }
            }
            invisible(NULL)
        },
        stop = function() {
            self$extinguish()
        },
        on = function(event, handler, pos = NULL) {
            assert_that(
                is.string(event),
                is.function(handler)
            )
            handlerId <- UUIDgenerate()
            private$handlerMap[[handlerId]] <- event
            private$add_handler(event, handler, pos, handlerId)
            
            invisible(handlerId)
        },
        off = function(handlerId) {
            assert_that(is.string(handlerId))
            private$remove_handler(handlerId)
            private$handlerMap[[handlerId]] <- NULL
            invisible(NULL)
        },
        trigger = function(event, ...) {
            assert_that(is.string(event))
            if (event %in% private$privateTriggers) {
                stop(event, ' and other protected events cannot be triggered while running', call. = FALSE)
            } else {
                private$p_trigger(event, server = self, ...)
            }
        },
        send = function(message, id) {
            private$send_ws(message, id)
            private$p_trigger('send', server = self, id = id, message = message)
            invisible(NULL)
        },
        attach = function(plugin, ..., force = FALSE) {
            name <- plugin$name
            assert_that(is.string(name))
            
            if (!force && self$has_plugin(name)) {
                stop('The ', name, ' plugin is already loaded. Use `force = TRUE` to reapply it.', call. = FALSE)
            }
            requires <- plugin$require
            if (!is.null(requires)) {
                assert_that(is.character(requires))
                exists <- vapply(requires, self$has_plugin, logical(1))
                if (!all(exists)) {
                    stop('The ', name, ' plugin requires the following plugins: ', paste(requires[!exists], collapse = ', '), '.', call. = FALSE)
                }
            }
            has_error <- try(plugin$onAttach(self, ...), silent = TRUE)
            if (is.error(has_error)) {
                stop('The ', name, ' plugin failed to attach with the following error: ', has_error, call. = FALSE)
            }
            private$add_plugin(plugin, name)
            invisible(NULL)
        },
        has_plugin = function(name) {
            name %in% names(private$pluginList)
        },
        header = function(name, value) {
            assert_that(is.string(name))
            if (missing(value)) return(private$headers[[name]])
            if (!is.null(value)) assert_that(is.string(value))
            private$headers[[name]] <- value
            invisible(NULL)
        },
        set_data = function(name, value) {
            assert_that(is.string(name))
            assign(name, value, envir = private$data)
            invisible(NULL)
        },
        get_data = function(name) {
            assert_that(is.string(name))
            private$data[[name]]
        },
        remove_data = function(name) {
            assert_that(is.string(name))
            rm(list = name, envir = private$data)
            invisible(NULL)
        },
        time = function(expr, then, after, loop = FALSE) {
            private$TIME$add(substitute(expr), then, after, loop, substituted = TRUE)
        },
        remove_time = function(id) {
            private$TIME$remove(id)
        },
        delay = function(expr, then) {
            private$DELAY$add(substitute(expr), then, substituted = TRUE)
        },
        remove_delay = function(id) {
            private$DELAY$remove(id)
        },
        async = function(expr, then) {
            private$ASYNC$add(substitute(expr), then, substituted = TRUE)
        },
        remove_async = function(id) {
            private$ASYNC$remove(id)
        },
        set_client_id_converter = function(converter) {
            assert_that(has_args(converter, 'request'))
            private$client_id <- converter
            invisible(NULL)
        },
        test_request = function(request) {
            private$request_logic(request)
        },
        test_header = function(request) {
            private$header_logic(request)
        },
        test_message = function(request, binary, message, withClose = TRUE) {
            id <- private$client_id(request)
            message_fun <- private$message_logic(id, request)
            message_fun(binary, message)
            if (withClose) {
                close_fun <- private$close_ws_logic(id, request)
                close_fun()
            }
        },
        test_websocket = function(request, message) {
            ws <- list(
                request = request,
                onMessage = function(func) {},
                onClose = function(func) {},
                send = function(message) {message(message)},
                close = function() {}
            )
            private$websocket_logic(ws)
            self$send(message)
            private$close_ws(private$client_id(request))
        }
    ),
    active = list(
        host = function(address) {
            if (missing(address)) return(private$HOST)
            assert_that(is.string(address))
            private$HOST <- address
        },
        port = function(n) {
            if (missing(n)) return(private$PORT)
            assert_that(is.count(n))
            private$PORT <- n
        },
        refreshRate = function(rate) {
            if (missing(rate)) return(private$REFRESHRATE)
            assert_that(is.number(rate))
            private$REFRESHRATE <- rate
        },
        refreshRateNB = function(rate) {
            if (missing(rate)) return(private$REFRESHRATENB)
            assert_that(is.number(rate))
            private$REFRESHRATENB <- rate
        },
        triggerDir = function(dir) {
            if (missing(dir)) return(private$TRIGGERDIR)
            if (!is.null(dir)) {
                assert_that(is.dir(dir))
            }
            private$TRIGGERDIR <- dir
        },
        plugins = function(plugin) {
            if (!missing(plugin)) {
                stop('Use the `attach` method to add plugins', call. = FALSE)
            }
            private$pluginList
        }
    ),
    private = list(
        # Data
        HOST = '127.0.0.1',
        PORT = 8080,
        REFRESHRATE = 0.001,
        REFRESHRATENB = 1,
        TRIGGERDIR = NULL,
        
        running = FALSE,
        nb_cycle = FALSE,
        quitting = FALSE,
        privateTriggers = c('start', 'resume', 'cycle-start', 'header', 
                            'before-request', 'request', 'after-request', 
                            'before-message', 'message', 'after-message', 
                            'websocket-closed', 'send', 'cycle-end', 'end'),
        data = NULL,
        headers = list(),
        handlers = NULL,
        handlerMap = list(),
        pluginList = list(),
        websockets = NULL,
        server = NULL,
        client_id = NULL,
        
        DELAY = NULL,
        TIME = NULL,
        ASYNC = NULL,
        
        # Methods
        run = function(block = TRUE, resume = FALSE, showcase = FALSE, ...) {
            assert_that(
                is.flag(block),
                is.flag(resume),
                is.flag(showcase)
            )
            if (!private$running) {
                private$running <- TRUE
                private$TIME$reset()
                private$p_trigger('start', server = self, ...)
                if (resume) {
                    private$p_trigger('resume', server = self, ...)
                }
                
                if (block) {
                    on.exit({
                        private$running <- FALSE
                        private$p_trigger('end', server = self)
                    })
                    private$run_blocking_server(showcase = showcase)
                } else {
                    private$run_allowing_server(showcase = showcase)
                }
            } else {
                warning('Server is already running and cannot be started')
            }
        },
        run_blocking_server = function(showcase = FALSE) {
            server <- startServer(
                self$host, 
                self$port, 
                list(
                    call = private$request_logic,
                    onHeaders = private$header_logic,
                    onWSOpen = private$websocket_logic
                )
            )
            
            on.exit(stopServer(server))
            
            if (showcase) {
                private$open_browser()
            }
            
            while (TRUE) {
                private$p_trigger('cycle-start', server = self)
                service()
                private$external_triggers()
                private$DELAY$eval(server = self)
                private$TIME$eval(server = self)
                private$ASYNC$eval(server = self)
                private$p_trigger('cycle-end', server = self)
                if (private$quitting) {
                    private$quitting <- FALSE
                    break
                }
                Sys.sleep(self$refreshRate)
            }
        },
        run_allowing_server = function(showcase = FALSE) {
            private$server <- startDaemonizedServer(
                self$host, 
                self$port, 
                list(
                    call = private$request_logic,
                    onHeaders = private$header_logic,
                    onWSOpen = private$websocket_logic
                )
            )
            
            if (showcase) {
                private$open_browser()
            }
            
            private$allowing_cycle()
        },
        allowing_cycle = function() {
            if (private$running) {
                private$nb_cycle <- TRUE # To hinder stopDeamonizedServer from crashing session
                private$p_trigger('cycle-start', server = self)
                private$external_triggers()
                private$DELAY$eval(server = self)
                private$TIME$eval(server = self)
                private$ASYNC$eval(server = self)
                private$p_trigger('cycle-end', server = self)
                private$nb_cycle <- FALSE
                later(function() {
                    private$allowing_cycle()
                }, private$REFRESHRATENB)
            }
        },
        request_logic = function(req) {
            id <- private$client_id(req)
            args <- unlist(
                unname(
                    private$p_trigger('before-request', server = self, id = id, 
                                      request = req)
                ), 
                recursive = FALSE
            )
            response <- private$p_trigger('request', server = self, id = id, request = req, arg_list = args)
            if (length(response) != 0) {
                response <- tail(response, 1)[[1]]
            }
            if (is.null(response)) {
                response <- notFound
            }
            if (!(is.list(response) && all(has_name(response, c('status', 'headers', 'body'))))) {
                response <- serverError
            }
            response$headers <- modifyList(private$headers, response$headers)
            private$p_trigger('after-request', server = self, id = id, request = req, response = response)
            response
        },
        header_logic = function(req) {
            id <- private$client_id(req)
            response <- private$p_trigger('header', server = self, id = id, request = req)
            if (length(response) == 0) {
                NULL
            } else {
                tail(response, 1)[[1]]
            }
        },
        websocket_logic = function(ws) {
            id <- private$client_id(ws$request)
            assign(id, ws, envir = private$websockets)
            
            ws$onMessage(private$message_logic(id, ws$request))
            ws$onClose(private$close_ws_logic(id, ws$request))
        },
        message_logic = function(id, request) {
            function(binary, msg) {
                args <- unlist(
                    unname(
                        private$p_trigger('before-message', server = self, 
                                          id = id, binary = binary, 
                                          message = msg, request = request)
                    ),
                    recursive = FALSE
                )
                if ('binary' %in% names(args)) binary <- args$binary
                if ('message' %in% names(args)) msg <- args$message
                args <- modifyList(args, list(binary = NULL, message = NULL))
                
                private$p_trigger('message', server = self, id = id, binary = binary, message = msg, request = request, arg_list = args)
                
                private$p_trigger('after-message', server = self, id = id, binary = binary, message = msg, request = request)
            }
        },
        close_ws_logic = function(id, request) {
            function() {
                private$p_trigger('websocket-closed', server = self, id = id, request = request)
            }
        },
        add_handler = function(event, handler, pos, id) {
            if (is.null(private$handlers[[event]])) {
                private$handlers[[event]] <- HandlerStack$new()
            }
            private$handlers[[event]]$add(handler, id, pos)
        },
        remove_handler = function(id) {
            event <- private$handlerMap[[id]]
            private$handlers[[event]]$remove(id)
        },
        add_plugin = function(plugin, name) {
            private$pluginList[[name]] <- plugin
        },
        p_trigger = function(event, ...) {
            if (!is.null(private$handlers[[event]])) {
                private$handlers[[event]]$dispatch(...)
            } else {
                setNames(list(), character())
            }
        },
        external_triggers = function() {
            if (is.null(private$TRIGGERDIR)) return()
            
            triggerFiles <- list.files(private$TRIGGERDIR, pattern = '*.rds', ignore.case = TRUE, full.names = TRUE)
            while (length(triggerFiles) > 0) {
                nextFile <- order(file.info(triggerFiles)$ctime)[1]
                event <- sub('\\.rds$', '', basename(triggerFiles[nextFile]), ignore.case = TRUE)
                args <- readRDS(triggerFiles[nextFile])
                unlink(triggerFiles[nextFile])
                if (!is.list(args)) {
                    warning('External triggers must be an rds file containing a list', call. = FALSE)
                    flush.console()
                } else {
                    args$event <- event
                    args$server <- self
                    do.call(private$p_trigger, args)
                }
                triggerFiles <- list.files(private$TRIGGERDIR, pattern = '*.rds', ignore.case = TRUE, full.names = TRUE)
            }
        },
        send_ws = function(message, id) {
            if (!is.raw(message)) {
                assert_that(
                    is.string(message),
                    is.scalar(message)
                )
            }
            if (missing(id) || is.null(id)) {
                id <- ls(envir = private$websockets)
            }
            for (i in id) {
                private$websockets[[i]]$send(message)
            }
        },
        close_ws = function(id) {
            private$websockets[[id]]$close()
            rm(list = id, envir = private$websockets)
        },
        open_browser = function() {
            url <- paste0('http://', private$HOST, ':', private$PORT, '/')
            browseURL(url)
        }
    )
)
