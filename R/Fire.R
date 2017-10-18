#' @include aaa.R
#' @include HandlerStack.R
#' @include loggers.R
NULL

#' Generate a New App Object
#' 
#' The Fire generator creates a new `Fire`-object, which is the class containing
#' all the app logic. The class is based on the [R6][R6::R6Class] OO-system and
#' is thus reference-based with methods and data attached to each object, in
#' contrast to the more well known S3 and S4 systems. A `fiery` server is event
#' driven, which means that it is build up and manipulated by adding event
#' handlers and triggering events. To learn more about the `fiery` event model,
#' read the [event documentation][events]. `fiery` servers can be modified 
#' directly or by attaching plugins. As with events, [plugins has its own
#' documentation][plugins].
#' 
#' @usage NULL
#' @format NULL
#' 
#' @section Initialization:
#' A new 'Fire'-object is initialized using the `new()` method on the generator:
#' 
#' \strong{Usage}
#' \tabular{l}{
#'  `app <- Fire$new(host = '127.0.0.1', port = 8080L)`
#' }
#' 
#' \strong{Arguments}
#' \tabular{lll}{
#'  `host` \tab  \tab A string overriding the default host (see the *Fields* section below)\cr
#'  `port` \tab  \tab An integer overriding the default port (see the *Fields* section below)
#' }
#' 
#' *Copying*
#' 
#' As `Fire` objects are using reference semantics new copies of an app cannot
#' be made simply be assigning it to a new variable. If a true copy of a `Fire`
#' object is desired, use the `clone()` method.
#' 
#' @section Fields:
#' \describe{
#'  \item{`host`}{A string giving a valid IPv4 address owned by the server, or `'0.0.0.0'` to listen on all addresses. The default is `'127.0.0.1'`}
#'  \item{`port`}{An integer giving the port number the server should listen on (defaults to `8080L`)}
#'  \item{`refresh_rate`}{The interval in seconds between run cycles when running a blocking server (defaults to `0.001`)}
#'  \item{`refresh_rate_nb`}{The interval in seconds between run cycles when running a non-bocking server (defaults to `1`)}
#'  \item{`trigger_dir`}{A valid folder where trigger files can be put when running a blocking server (defaults to `NULL`)}
#'  \item{`plugins`}{A named list of the already attached plugins. **Static** - can only be modified using the `attach()` method.}
#'  \item{`root`}{The location of the app. Setting this will remove the root value from requests (or decline them with `400` if the request does not match the root). E.g. the path of a request will be changed from `/demo/test` to `/test` if `root == '/demo'`}
#'  \item{`access_log_format`}{A [glue][glue::glue] string defining how requests will be logged. For standard formats see [common_log_format] and [combined_log_format]. Defaults to the *Common Log Format*}
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{`ignite(block = TRUE, showcase = FALSE, ...)`}{Begins the server, either blocking the console if `block = TRUE` or not. If `showcase = TRUE` a browser window is opened directing at the server address. `...` will be redirected to the `start` handler(s)}
#'  \item{`start(block = TRUE, showcase = FALSE, ...)`}{A less dramatic synonym of for `ignite()`}
#'  \item{`reignite(block = TRUE, showcase = FALSE, ...)`}{As `ignite` but additionally triggers the `resume` event after the `start` event}
#'  \item{`resume(block = TRUE, showcase = FALSE, ...)`}{Another less dramatic synonym, this time for `reignite()`}
#'  \item{`extinguish()`}{Stops a running server}
#'  \item{`stop()`}{Boring synonym for `extinguish()`}
#'  \item{`is_running()`}{Check if the server is currently running}
#'  \item{`on(event, handler, pos = NULL)`}{Add a `handler` function to to an `event` at the given position (`pos`) in the handler stack. Returns a string uniquely identifying the handler. See the [event documentation][events] for more information.}
#'  \item{`off(handlerId)`}{Remove the handler tied to the given `id`}
#'  \item{`trigger(event, ...)`}{Triggers an `event` passing the additional arguments to the potential handlers}
#'  \item{`send(message, id)`}{Sends a websocket `message` to the client with the given `id`, or to all connected clients if `id` is missing}
#'  \item{`log(event, message, request, ...)`}{Send a `message` to the logger. The `event` defines the type of message you are passing on, while `request` is the related `Request` object if applicable.}
#'  \item{`close_ws_con(id)`}{Closes the websocket connection started from the client with the given `id`, firing the `websocket-closed` event}
#'  \item{`attach(plugin, ..., force = FALSE)`}{Attaches a `plugin` to the server. See the [plugin documentation][plugins] for more information. Plugins can only get attached once unless `force = TRUE`}
#'  \item{`has_plugin(name)`}{Check whether a plugin with the given `name` has been attached}
#'  \item{`header(name, value)`}{Add a global `header` to the server that will be set on all responses. Remove by setting `value = NULL`}
#'  \item{`set_data(name, value)`}{Adds data to the servers internal data store}
#'  \item{`get_data(name)`}{Extracts data from the internal data store}
#'  \item{`remove_data(name)`}{Removes the data with the given `name` from the internal data store}
#'  \item{`time(expr, then, after, loop = FALSE)`}{Add a timed evaluation (`expr`) that will be evaluated after the given number of seconds (`after`), potentially repeating if `loop = TRUE`. After the expression has evaluated the `then` function will get called with the result of the expression and the server object as arguments.}
#'  \item{`remove_time(id)`}{Removes the timed evaluation identified by the `id` (returned when adding the evaluation)}
#'  \item{`delay(expr, then)`}{Similar to `time()`, except the `expr` is evaluated immediately at the end of the loop cycle ([see here][delay_doc] for detailed explanation of delayed evaluation in fiery).}
#'  \item{`remove_delay(id)`}{Removes the delayed evaluation identified by the `id`}
#'  \item{`async(expr, then)`}{As `delay()` and `time()` except the expression is evaluated asynchronously. The progress of evaluation is checked at the end of each loop cycle}
#'  \item{`remove_async(id)`}{Removes the async evaluation identified by the `id`. The evaluation is not necessarily stopped but the then function will not get called.}
#'  \item{`set_client_id_converter(converter)`}{Sets the function that converts an HTTP request into a specific client id}
#'  \item{`set_logger(logger)`}{Sets the function that takes care of logging}
#'  \item{`set_client_id_converter(converter)`}{Sets the function that converts an HTTP request into a specific client id}
#'  \item{`clone()`}{Create a copy of the full `Fire` object and return that}
#' }
#' 
#' @importFrom R6 R6Class
#' @importFrom assertthat is.string is.count is.number has_args assert_that is.dir is.flag has_name is.error
#' @importFrom httpuv startServer service startDaemonizedServer stopDaemonizedServer stopServer
#' @importFrom uuid UUIDgenerate
#' @importFrom utils browseURL
#' @importFrom later later
#' @importFrom stats setNames
#' @importFrom reqres Request
#' @importFrom stringi stri_pad_left
#' 
#' @export
#' @docType class
#' 
#' @seealso [events] describes how the server event cycle works
#' 
#' [plugins] describes how to use plugins to modify the server
#' 
#' @examples 
#' # Create a New App
#' app <- Fire$new(port = 4689)
#' 
#' # Setup the data every time it starts
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
      private$logger <- logger_null()
      private$DELAY <- DelayStack$new(self)
      private$TIME <- TimeStack$new(self)
      private$ASYNC <- AsyncStack$new(self)
      private$LOG_QUEUE <- DelayStack$new(self)
    },
    format = function(...) {
      text <- c(
        '\U0001f525 A fiery webserver',
        '\U0001f525  \U0001f4a5   \U0001f4a5   \U0001f4a5'
      )
      mat <- matrix(c('Running on', ': ', paste0(self$host, ':', self$port, self$root)), ncol = 3)
      plugins <- names(private$pluginList)
      if (is.null(plugins)) plugins <- 'none'
      mat <- rbind(mat, c('Plugins attached', ': ', plugins[1]))
      mat <- rbind(mat, matrix(c(rep('  ', (length(plugins) - 1)*2), plugins[-1]), ncol = 3))
      handlers <- lapply(private$handlers, function(x) x$length())
      if (length(handlers) == 0) {
        mat <- rbind(mat, c('Event handlers added', ': ', 'none'))
      } else {
        mat <- rbind(mat, c('Event handlers added', '', ''))
        order <- match(names(handlers), private$privateTriggers)
        order[is.na(order)] <- seq_len(sum(is.na(order))) + max(order, na.rm = TRUE)
        handlers <- handlers[order(order)]
        mat <- rbind(mat, matrix(c(names(handlers), rep(': ', length(handlers)), as.character(unlist(handlers))), ncol = 3))
      }
      mat[, 1] <- stri_pad_left(mat[, 1], max(nchar(mat[,1])))
      c(text, paste0('\U0001f525 ', apply(mat, 1, paste, collapse = '')))
    },
    ignite = function(block = TRUE, showcase = FALSE, ..., silent = FALSE) {
      private$run(block = block, showcase = showcase, ..., silent = silent)
      invisible(NULL)
    },
    start = function(block = TRUE, showcase = FALSE, ..., silent = FALSE) {
      self$ignite(block = block, showcase = showcase, ..., silent = silent)
    },
    reignite = function(block = TRUE, showcase = FALSE, ..., silent = FALSE) {
      private$run(block = block, resume = TRUE, showcase = showcase, ..., silent = silent)
      invisible(NULL)
    },
    resume = function(block = TRUE, showcase = FALSE, ..., silent = FALSE) {
      self$reignite(block = block, showcase = showcase, ..., silent = silent)
    },
    extinguish = function() {
      if (private$running) {
        if (!is.null(private$server)) {
          if (private$nb_cycle) {
            warning('Cannot stop server from within a non-blocking event cycle', call. = FALSE)
            return(invisible(NULL))
          } else {
            private$running <- FALSE
            private$p_trigger('end', server = self)
            stopDaemonizedServer(private$server)
            private$server <- NULL
            self$log('stop', paste0(self$host, ':', self$port, self$root))
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
        stop(event, ' and other protected events cannot be triggered manually', call. = FALSE)
      } else {
        private$p_trigger(event, server = self, ...)
      }
    },
    send = function(message, id) {
      private$send_ws(message, id)
      private$p_trigger('send', server = self, id = id, message = message)
      invisible(NULL)
    },
    close_ws_con = function(id) {
      assert_that(is.string(id))
      ws <- private$websockets[[id]]
      if (!is.null(ws)) {
        private$close_ws(id)
      }
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
      has_error <- tri(plugin$on_attach(self, ...))
      if (is.error_cond(has_error)) {
        stop('The ', name, ' plugin failed to attach with the following error: ', conditionMessage(has_error), call. = FALSE)
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
    set_logger = function(logger) {
      assert_that(is.function(logger))
      assert_that(has_args(logger, c('event', 'message', 'request', '...')))
      private$logger <- logger
      invisible(NULL)
    },
    log = function(event, message, request = NULL, ...) {
      time <- Sys.time()
      if (private$running) {
        private$LOG_QUEUE$add(NULL, function(...) private$logger(event, message, request, time, ...))
      } else {
        private$logger(event, message, request, time, ...)
      }
      invisible(NULL)
    },
    is_running = function() {
      private$running
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
    test_websocket = function(request, message, close = TRUE) {
      ws <- list(
        request = request,
        onMessage = function(func) {},
        onClose = function(func) {},
        send = function(message) {message(message)},
        close = function() {message('closing')}
      )
      private$websocket_logic(ws)
      self$send(message, private$client_id(request))
      if (close) private$close_ws(private$client_id(request))
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
    refresh_rate = function(rate) {
      if (missing(rate)) return(private$REFRESHRATE)
      assert_that(is.number(rate))
      private$REFRESHRATE <- rate
    },
    refresh_rate_nb = function(rate) {
      if (missing(rate)) return(private$REFRESHRATENB)
      assert_that(is.number(rate))
      private$REFRESHRATENB <- rate
    },
    trigger_dir = function(dir) {
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
    },
    root = function(path) {
      if (missing(path)) return(private$ROOT)
      assert_that(is.string(path))
      path <- sub('/$', '', path)
      if (path != '') path <- paste0('/', sub('^/+', '', path))
      private$ROOT <- path
    },
    access_log_format = function(format) {
      if (missing(format)) return(private$ACCESS_LOG_FORMAT)
      assert_that(is.string(format))
      private$ACCESS_LOG_FORMAT <- format
    }
  ),
  private = list(
    # Data
    HOST = '127.0.0.1',
    PORT = 8080,
    REFRESHRATE = 0.001,
    REFRESHRATENB = 1,
    TRIGGERDIR = NULL,
    ROOT = '',
    ACCESS_LOG_FORMAT = common_log_format,
    
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
    logger = NULL,
    
    DELAY = NULL,
    TIME = NULL,
    ASYNC = NULL,
    LOG_QUEUE = NULL,
    
    # Methods
    run = function(block = TRUE, resume = FALSE, showcase = FALSE, ..., silent = FALSE) {
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
          if (!silent) message('Fire restarted at ', self$host, ':', self$port, self$root)
          self$log('resume', paste0(self$host, ':', self$port, self$root))
        } else {
          if (!silent) message('Fire started at ', self$host, ':', self$port, self$root)
          self$log('start', paste0(self$host, ':', self$port, self$root))
        }
        
        if (block) {
          on.exit({
            private$running <- FALSE
            private$p_trigger('end', server = self)
            self$log('stop', paste0(self$host, ':', self$port, self$root))
          })
          private$run_blocking_server(showcase = showcase)
        } else {
          private$run_allowing_server(showcase = showcase)
        }
      } else {
        self$log('warning', 'Server is already running and cannot be started')
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
        private$safe_call(private$DELAY$eval(server = self))
        private$safe_call(private$TIME$eval(server = self))
        private$safe_call(private$ASYNC$eval(server = self))
        tri(private$LOG_QUEUE$eval(server = self))
        private$p_trigger('cycle-end', server = self)
        if (private$quitting) {
          private$quitting <- FALSE
          break
        }
        Sys.sleep(self$refresh_rate)
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
        private$safe_call(private$DELAY$eval(server = self))
        private$safe_call(private$TIME$eval(server = self))
        private$safe_call(private$ASYNC$eval(server = self))
        tri(private$LOG_QUEUE$eval(server = self))
        private$p_trigger('cycle-end', server = self)
        private$nb_cycle <- FALSE
        later(function() {
          private$allowing_cycle()
        }, private$REFRESHRATENB)
      }
    },
    mount_request = function(req) {
      if (!grepl(paste0('^', self$root, '(/|$)'), req$PATH_INFO)) stop('URL not matching mount point', call. = FALSE)
      req$SCRIPT_NAME <- self$root
      req$PATH_INFO <- sub(paste0('^', self$root, ''), '', req$PATH_INFO)
      req
    },
    request_logic = function(req) {
      start_time <- Sys.time()
      request <- tri(private$mount_request(req))
      if (is.error_cond(request)) {
        req <- Request$new(req)
        id <- private$client_id(req)
        response <- req$respond()
        response$status_with_text(400L)
        self$log('error', conditionMessage(request), req)
      } else {
        req <- Request$new(request)
        id <- private$client_id(req)
        args <- unlist(
          unname(
            private$p_trigger('before-request', server = self, id = id, 
                              request = req)
          ), 
          recursive = FALSE
        )
        private$p_trigger('request', server = self, id = id, request = req, arg_list = args)
        response <- req$respond()
        for (i in names(private$headers)) response$set_header(i, private$headers[[i]])
        response <- response$as_list()
        private$p_trigger('after-request', server = self, id = id, request = req)
      }
      end_time <- Sys.time()
      self$log('request', glue_log(
        list(start_time = start_time, end_time = end_time, request = req, response = req$response, id = id),
        self$access_log_format
      ), req)
      response
    },
    header_logic = function(req) {
      start_time <- Sys.time()
      request <- tri(private$mount_request(req))
      if (is.error_cond(request)) {
        req <- Request$new(req)
        id <- private$client_id(req)
        response <- req$respond()
        response$status_with_text(400L)
        self$log('error', conditionMessage(request), req)
      } else {
        req <- Request$new(request)
        id <- private$client_id(req)
        response <- private$p_trigger('header', server = self, id = id, request = req)
        response <- if (length(response) == 0) {
          NULL
        } else {
          continue <- tail(response, 1)[[1]]
          assert_that(is.flag(continue))
          if (continue) {
            NULL
          } else {
            self$log('request', 'denied after header', req)
            req$respond()$as_list()
          }
        }
      }
      if (!is.null(response)) {
        end_time <- Sys.time()
        self$log('request', glue_log(
          list(start_time = start_time, end_time = end_time, request = req, response = req$response, id = id),
          self$access_log_format
        ), req)
      }
      response
    },
    websocket_logic = function(ws) {
      request <- tri(private$mount_request(ws$request))
      if (is.error_cond(request)) {
        self$log('error', conditionMessage(request))
        ws$close()
        return()
      } else {
        req <- Request$new(request)
      }
      id <- private$client_id(req)
      assign(id, ws, envir = private$websockets)
      self$log('websocket', paste0('connection established to ', id), req)
      
      ws$onMessage(private$message_logic(id, req))
      ws$onClose(private$close_ws_logic(id, req))
    },
    message_logic = function(id, request) {
      function(binary, msg) {
        start <- Sys.time()
        args <- unlist(
          unname(
            private$p_trigger('before-message', server = self, 
                              id = id, binary = binary, 
                              message = msg, request = request)
          ),
          recursive = FALSE
        )
        if (is.null(args)) args <- structure(list(), names = character())
        if ('binary' %in% names(args)) binary <- args$binary
        if ('message' %in% names(args)) msg <- args$message
        args <- modifyList(args, list(binary = NULL, message = NULL))
        
        private$p_trigger('message', server = self, id = id, binary = binary, message = msg, request = request, arg_list = args)
        
        private$p_trigger('after-message', server = self, id = id, binary = binary, message = msg, request = request)
        
        self$log('websocket', paste0('from ', id, ' processed in ', format(Sys.time() - start, digits = 3)), request, message = msg)
      }
    },
    close_ws_logic = function(id, request) {
      function() {
        private$p_trigger('websocket-closed', server = self, id = id, request = request)
        self$log('websocket', paste0('connection to ', id, ' closed from the client'), request)
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
        res <- private$safe_call(private$handlers[[event]]$dispatch(...))
        for (val in res) if (is.error_cond(val)) self$log('error', conditionMessage(val))
      } else {
        res <- setNames(list(), character())
      }
      res
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
          self$log('warning', 'External triggers must be an rds file containing a list')
        } else {
          args$event <- event
          args$server <- self
          do.call(private$p_trigger, args)
        }
        triggerFiles <- list.files(private$TRIGGERDIR, pattern = '*.rds', ignore.case = TRUE, full.names = TRUE)
      }
    },
    safe_call = function(expr) {
      withCallingHandlers(
        tryCatch(expr, error = function(e) {
          self$log('error', conditionMessage(e))
        }),
        warning = function(w) {
          self$log('warning', conditionMessage(w))
          invokeRestart('muffleWarning')
        },
        message = function(m) {
          self$log('message', conditionMessage(m))
          invokeRestart('muffleMessage')
        }
      )
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
      } else {
        id <- intersect(id, ls(envir = private$websockets))
      }
      if (length(id) == 0) return(NULL)
      for (i in id) {
        private$websockets[[i]]$send(message)
      }
      self$log('websocket', paste0('send to ', paste(id, collapse = ', ')))
    },
    close_ws = function(id) {
      ws <- private$websockets[[id]]
      if (!is.null(ws)) {
        try(ws$close(), silent = TRUE)
        rm(list = id, envir = private$websockets)
        self$log('websocket', paste0('connection to ', id, ' closed from the server'))
      }
    },
    open_browser = function() {
      url <- paste0('http://', private$HOST, ':', private$PORT, '/')
      browseURL(url)
    }
  )
)
