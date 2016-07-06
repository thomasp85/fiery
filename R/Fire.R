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
#' 
#' @section Fields:
#' \describe{
#'  \item{\code{host}}{A string giving a valid IPv4 address owned by the server, or '0.0.0.0' (the default) to listen on all addresses}
#'  \item{\code{port}}{An integer giving the port number the server should listen on (defaults to 80L)}
#'  \item{\code{refreshRate}}{The interval in seconds between run cycles when running a blocking server (defaults to 0.001)}
#'  \item{\code{triggerDir}}{A valid folder where trigger files can be put when running a blocking server (defaults to NULL)}
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
#'  \item{\code{attach(plugin, ...)}}{Attaches a plugin to the server. A plugin is an R6 object with an \code{onAttach} method}
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
#'  \item{\code{test_request(request)}}{Test the result of recieving a specific HTTP request}
#'  \item{\code{test_header(request)}}{Test the result of recieving a specific HTTP header}
#'  \item{\code{test_message(request, binary, message, withClose = TRUE)}}{Test the result of recieving a message over websocket and potentially closing the connection afterwards}
#'  \item{\code{test_websocket(request, message)}}{Test the result of sending a message over websocket}
#' }
#' 
#' @importFrom R6 R6Class
#' @importFrom assertthat is.string is.count is.number has_args assert_that is.dir is.flag has_name
#' @importFrom httpuv startServer service startDaemonizedServer stopDaemonizedServer stopServer
#' @importFrom uuid UUIDgenerate
#' @importFrom utils browseURL
#' 
#' @export
#' @docType class
#' 
#' @examples 
#' # Create a New App
#' app <- Fire$new()
#' app$port <- 4689
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
        initialize = function() {
            private$data <- new.env(parent = emptyenv())
            private$handlers <- new.env(parent = emptyenv())
            private$websockets <- new.env(parent = emptyenv())
            private$client_id <- client_to_id
            private$DELAY <- DelayStack$new()
            private$TIME <- TimeStack$new()
            private$ASYNC <- AsyncStack$new()
        },
        ignite = function(block = TRUE, showcase = FALSE, ...) {
            private$run(block = block, showcase = showcase, ...)
            invisible(NULL)
        },
        start = function(block = TRUE, showcase = FALSE, ...) {
            self$ignite(block = block, showcase = showcase, ...)
        },
        reignite = function(block = TRUE, showcase = FALSE, ...) {
            private$run(block = block, resume = TRUE, showcase = showcase, ...)
            invisible(NULL)
        },
        resume = function(block = TRUE, showcase = FALSE, ...) {
            self$reignite(block = block, showcase = showcase, ...)
        },
        extinguish = function() {
            if (private$running) {
                if (!is.null(private$server)) {
                    private$running <- FALSE
                    private$p_trigger('end', server = self)
                    stopDaemonizedServer(private$server)
                    private$server <- NULL
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
            private$p_trigger('send', server = self, message = message, id = id)
            invisible(NULL)
        },
        attach = function(plugin, ...) {
            plugin$onAttach(self, ...)
            invisible(NULL)
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
        triggerDir = function(dir) {
            if (missing(dir)) return(private$TRIGGERDIR)
            if (!is.null(dir)) {
                assert_that(is.dir(dir))
            }
            private$TRIGGERDIR <- dir
        }
    ),
    private = list(
        # Data
        HOST = '127.0.0.1',
        PORT = 8080,
        REFRESHRATE = 0.001,
        TRIGGERDIR = NULL,
        
        running = FALSE,
        quitting = FALSE,
        privateTriggers = c('start', 'resume', 'end', 'cycle-start', 
                            'cycle-end', 'header', 'before-request', 'request', 
                            'after-request', 'before-message', 'message', 
                            'after-message', 'websocket-closed', 'send'),
        data = NULL,
        headers = list(),
        handlers = NULL,
        handlerMap = list(),
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
            if (is.null(args)) {
                args <- list(
                    event = 'request',
                    server = self,
                    id = id,
                    request = req
                )
            } else {
                args <- modifyList(args, list(
                    event = 'request',
                    server = self,
                    id = id,
                    request = req
                ))
            }
            response <- tail(do.call(private$p_trigger, args), 1)[[1]]
            if (is.null(response)) response <- notFound
            if (!(is.list(response) && all(has_name(response, c('status', 'headers', 'body'))))) {
                response <- serverError
            }
            response$headers <- modifyList(private$headers, response$headers)
            private$p_trigger('after-request', server = self, id = id, request = req, response = response)
            response
        },
        header_logic = function(req) {
            id <- private$client_id(req)
            private$p_trigger('header', server = self, id = id, request = req)
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
                args <- modifyList(list(binary = binary, message = msg), args)
                args <- modifyList(args, list(
                    event = 'message',
                    server = self,
                    id = id,
                    request = request
                ))
                do.call(private$p_trigger, args)
                
                private$p_trigger('after-message', server = self, id = id, binary = args$binary, message = args$message, request = request)
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
        p_trigger = function(event, ...) {
            if (!is.null(private$handlers[[event]])) {
                private$handlers[[event]]$dispatch(...)
            } else {
                `names<-`(list(), character())
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