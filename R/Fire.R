#' @include aaa.R
#' @include HandlerStack.R
#' @include loggers.R
NULL

#' Generate a New App Object
#' 
#' @description 
#' The Fire generator creates a new `Fire`-object, which is the class containing
#' all the app logic. The class is based on the [R6][R6::R6Class] OO-system and
#' is thus reference-based with methods and data attached to each object, in
#' contrast to the more well known S3 and S4 systems. A `fiery` server is event
#' driven, which means that it is build up and manipulated by adding event
#' handlers and triggering events. To learn more about the `fiery` event model,
#' read the [event vignette](https://fiery.data-imaginist.com/articles/events.html). 
#' `fiery` servers can be modified directly or by attaching plugins. As with 
#' events, [plugins has its own vignette](https://fiery.data-imaginist.com/articles/plugins.html).
#' 
#' ## Initialization
#' A new 'Fire'-object is initialized using the `new()` method on the generator:
#' 
#' \tabular{l}{
#'  `app <- Fire$new(host = '127.0.0.1', port = 8080L)`
#' }
#' 
#' ## Copying
#' 
#' As `Fire` objects are using reference semantics new copies of an app cannot
#' be made simply be assigning it to a new variable. If a true copy of a `Fire`
#' object is desired, use the `clone()` method.
#' 
#' @importFrom R6 R6Class
#' @importFrom httpuv startServer service startDaemonizedServer stopDaemonizedServer stopServer
#' @importFrom uuid UUIDgenerate
#' @importFrom utils browseURL
#' @importFrom later later
#' @importFrom stats setNames
#' @importFrom reqres Request
#' @importFrom stringi stri_pad_left
#' 
#' @export
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
    #' @description Create a new `Fire` app
    #' @param host A string overriding the default host
    #' @param port An port number overriding the default port
    #' @return A `Fire` object
    initialize = function(host = '127.0.0.1', port = 8080) {
      self$host <- host
      self$port <- port
      private$data <- new.env(parent = emptyenv())
      private$handlers <- new.env(parent = emptyenv())
      private$websockets <- new.env(parent = emptyenv())
      private$client_id <- client_to_id
      private$DELAY <- DelayStack$new(self)
      private$TIME <- TimeStack$new(self)
      private$ASYNC <- AsyncStack$new(self)
      private$LOG_QUEUE <- DelayStack$new(self)
    },
    #' @description Human readable description of the app
    #' @param ... ignored
    #' @return A character vector
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
    #' @description Begin running the server. Will trigger the `start` event
    #' @param block Should the console be blocked while running (alternative is to run in the background)
    #' @param showcase Should the default browser open up at the server address
    #' @param ... Arguments passed on to the `start` handler
    #' @param silent Should startup messaging by silenced
    ignite = function(block = TRUE, showcase = FALSE, ..., silent = FALSE) {
      private$run(block = block, showcase = showcase, ..., silent = silent)
      invisible(NULL)
    },
    #' @description Synonymous method to `ignite()`
    #' @param ... passed on to `ignite()`
    start = function(...) {
      self$ignite(...)
    },
    #' @description Resume a session. This is equivalent to `ignite()` but will also trigger the `resume` event
    #' @param ... passed on to `ignite()`
    reignite = function(...) {
      private$run(..., resume = TRUE)
      invisible(NULL)
    },
    #' @description Synonymous method to `reignite()`
    #' @param ... passed on to `ignite()`
    resume = function(...) {
      self$reignite(...)
    },
    #' @description Stop the server. Will trigger the `end` event
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
    #' @description Synonymous method to `extinguish()`
    stop = function() {
      self$extinguish()
    },
    #' @description Add a handler to an event. See the [*The event cycle in fiery* vignette](https://fiery.data-imaginist.com/articles/events.html) for more information.
    #' @param event The name of the event that should trigger the handler
    #' @param handler The handler function that should be triggered
    #' @param pos The position in the handler stack to place it at. `NULL` will place it at the end.
    #' @return A unique string identifying the handler
    on = function(event, handler, pos = NULL) {
      check_string(event)
      check_function(handler)
      handlerId <- UUIDgenerate()
      private$handlerMap[[handlerId]] <- event
      private$add_handler(event, handler, pos, handlerId)
      
      invisible(handlerId)
    },
    #' @description Remove an event handler from the app.
    #' @param handlerId The unique id identifying the handler
    off = function(handlerId) {
      check_string(handlerId)
      private$remove_handler(handlerId)
      private$handlerMap[[handlerId]] <- NULL
      invisible(NULL)
    },
    #' @description Trigger an event in the app. This will cause any handler attached to the event to be called. See the [*The event cycle in fiery* vignette](https://fiery.data-imaginist.com/articles/events.html) for more information.
    #' @param event The name of the event
    #' @param ... Arguments passed on to the handlers
    #' @return A named list containing the return values of all handlers attached to the event
    trigger = function(event, ...) {
      check_string(event)
      if (event %in% private$privateTriggers) {
        cli::cli_abort('{val {event}} and other protected events cannot be triggered manually')
      } else {
        private$p_trigger(event, server = self, ...)
      }
    },
    #' @description Send a Websocket message to a client. Will trigger the `send` event.
    #' @param message The message to send
    #' @param id The id of the client to send to. If missing, the message will be send to all clients
    send = function(message, id) {
      private$send_ws(message, id)
      private$p_trigger('send', server = self, id = id, message = message)
      invisible(NULL)
    },
    #' @description Close a Websocket connection. Will trigger the `websocket-closed` event
    #' @param id The id of the client to close the websocket connection to
    close_ws_con = function(id) {
      check_string(id)
      ws <- private$websockets[[id]]
      if (!is.null(ws)) {
        private$close_ws(id)
      }
    },
    #' @description Attach a plugin to the app. See the [*Creating and using fiery plugins* vignette](https://fiery.data-imaginist.com/articles/plugins.html) for more information
    #' @param plugin The plugin to attach
    #' @param ... Arguments to pass into the plugins `on_attach()` method
    #' @param force If the plugin has already been attached an error is thrown, unless `force = TRUE` which tells the app to reattach it
    attach = function(plugin, ..., force = FALSE) {
      name <- plugin$name
      check_string(name, arg = 'plugin$name')
      
      if (!force && self$has_plugin(name)) {
        cli::cli_abort(c(
          'The {.arg {name}} plugin is already loaded.',
          i = 'Use {.code force = TRUE} to reapply it.'
        ))
      }
      requires <- plugin$require
      if (!is.null(requires)) {
        check_character(requires)
        exists <- vapply(requires, self$has_plugin, logical(1))
        if (!all(exists)) {
          cli::cli_abort('The {.arg {name}} plugin requires the following {cli::qty(requires[!exists])} plugin{?s}: {requires[!exists]}')
        }
      }
      try_fetch(
        plugin$on_attach(self, ...),
        error = function(cnd) {
          cli::cli_abort('The {.arg {name}} plugin failed to attach to the app', parent = cnd)
        }
      )
      private$add_plugin(plugin, name)
      invisible(NULL)
    },
    #' @description Check if the app has a plugin attached
    #' @param name The name of the plugin
    #' @return A boolean indicating if the given plugin is already attached
    has_plugin = function(name) {
      name %in% names(private$pluginList)
    },
    #' @description Add a global http header that will be applied to all responses
    #' @param name The name of the header
    #' @param value The value of the header. Use `NULL` to remove the global header
    header = function(name, value) {
      check_string(name)
      if (missing(value)) return(private$headers[[name]])
      check_string(value, allow_null = TRUE)
      private$headers[[name]] <- value
      invisible(NULL)
    },
    #' @description Add data to the global data store
    #' @param name The name identifying the data
    #' @param value The data to add 
    set_data = function(name, value) {
      check_string(name)
      assign(name, value, envir = private$data)
      invisible(NULL)
    },
    #' @description Retrieve data from the global data store
    #' @param name The name identifying the data
    #' @return The data requested. Returns `NULL` if the store does not contain the requested data
    get_data = function(name) {
      check_string(name)
      private$data[[name]]
    },
    #' @description Remove data from the global data store
    #' @param name The name identifying the data to be removed
    remove_data = function(name) {
      check_string(name)
      rm(list = name, envir = private$data)
      invisible(NULL)
    },
    #' @description Add a timed evaluation that will be evaluated after the given number of seconds. See the [*Delaying code execution in Fiery* vignette](https://fiery.data-imaginist.com/articles/delayed.html) for more information
    #' @param expr The expression to evaluate when the time has passed
    #' @param then A handler to call once `expr` has been evaluated
    #' @param after The time in second to wait before evaluating `expr`
    #' @param loop Should `expr` be called repeatedly with the interval given by `after`
    #' @return A unique id identifying the handler
    time = function(expr, then, after, loop = FALSE) {
      private$TIME$add(substitute(expr), then, after, loop, substituted = TRUE)
    },
    #' @description Remove a timed evaluation
    #' @param id The unique id identifying the handler
    remove_time = function(id) {
      private$TIME$remove(id)
    },
    #' @description Add a delayed evaluation to be evaluated immediately at the end of the loop cycle. See the [*Delaying code execution in Fiery* vignette](https://fiery.data-imaginist.com/articles/delayed.html) for more information
    #' @param expr The expression to evaluate at the end of the cycle
    #' @param then A handler to call once `expr` has been evaluated
    #' @return A unique id identifying the handler
    delay = function(expr, then) {
      private$DELAY$add(substitute(expr), then, substituted = TRUE)
    },
    #' @description Remove a delayed evaluation
    #' @param id The unique id identifying the handler
    remove_delay = function(id) {
      private$DELAY$remove(id)
    },
    #' @description Add an asynchronous evaluation to be evaluated in another process without blocking the server. See the [*Delaying code execution in Fiery* vignette](https://fiery.data-imaginist.com/articles/delayed.html) for more information
    #' @param expr The expression to evaluate at the end of the cycle
    #' @param then A handler to call once `expr` has been evaluated
    #' @return A unique id identifying the handler
    async = function(expr, then) {
      private$ASYNC$add(substitute(expr), then, substituted = TRUE)
    },
    #' @description Remove an async evaluation
    #' @param id The unique id identifying the handler
    remove_async = function(id) {
      private$ASYNC$remove(id)
    },
    #' @description Sets the function that converts an HTTP request into a specific client id
    #' @param converter A function with the argument `request`
    set_client_id_converter = function(converter) {
      check_args(converter, 'request')
      private$client_id <- converter
      invisible(NULL)
    },
    #' @description Sets the logging function to use
    #' @param logger A function with the arguments `event`, `message`, `request`, and `...`
    set_logger = function(logger) {
      check_function(logger)
      check_args(logger, c('event', 'message', 'request', '...'))
      private$logger <- list(logger)
      invisible(NULL)
    },
    #' @description Log a message with the logger attached to the app. See [loggers] for build in functionality
    #' @param event The event associated with the message
    #' @param message The message to log
    #' @param request The `Request` object associated with the message, if any.
    #' @param ... Additional arguments passed on to the logger.
    log = function(event, message, request = NULL, ...) {
      time <- Sys.time()
      force(message)
      if (private$running) {
        private$LOG_QUEUE$add(NULL, function(...) private$logger[[1]](event, message, request, time, ...))
      } else {
        private$logger[[1]](event, message, request, time, ...)
      }
      invisible(NULL)
    },
    #' @description Test if an app is running
    is_running = function() {
      private$running
    },
    #' @description Send a request directly to the request logic of a non-running app. Only intended for testing the request logic
    #' @param request The request to send
    test_request = function(request) {
      private$request_logic(request)
    },
    #' @description Send a request directly to the header logic of a non-running app. Only intended for testing the request logic
    #' @param request The request to send
    test_header = function(request) {
      private$header_logic(request)
    },
    #' @description Send a message directly **to** the message logic of a non-running app. Only intended for testing the websocket logic
    #' @param request The request to use to establish the connection
    #' @param binary Is the message send in binary or character format
    #' @param message The message to send. If `binary = FALSE` a character vector, if `binary = TRUE` a raw vector
    #' @param withClose Should the websocket connection be closed at the end by the client
    test_message = function(request, binary, message, withClose = TRUE) {
      id <- private$client_id(request)
      message_fun <- private$message_logic(id, request)
      message_fun(binary, message)
      if (withClose) {
        close_fun <- private$close_ws_logic(id, request)
        close_fun()
      }
    },
    #' @description Send a message directly **from** a non-running app. Only intended for testing the websocket logic
    #' @param request The request to use to establish the connection
    #' @param message The message to send from the app
    #' @param close Should the websocket connection be closed at the end by the server
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
    #' @field host A string giving a valid IPv4 address owned by the server, or `'0.0.0.0'` to listen on all addresses. The default is `'127.0.0.1'`
    host = function(address) {
      if (missing(address)) return(private$HOST)
      check_string(address)
      private$HOST <- address
    },
    #' @field port An integer giving the port number the server should listen on (defaults to `8080L`)
    port = function(n) {
      if (missing(n)) return(private$PORT)
      check_number_whole(n, min = 1)
      private$PORT <- n
    },
    #' @field refresh_rate The interval in seconds between run cycles when running a blocking server (defaults to `0.001`)
    refresh_rate = function(rate) {
      if (missing(rate)) return(private$REFRESHRATE)
      check_number_decimal(rate)
      private$REFRESHRATE <- rate
    },
    #' @field refresh_rate_nb The interval in seconds between run cycles when running a non-blocking server (defaults to `1`)
    refresh_rate_nb = function(rate) {
      if (missing(rate)) return(private$REFRESHRATENB)
      check_number_decimal(rate)
      private$REFRESHRATENB <- rate
    },
    #' @field trigger_dir A valid folder where trigger files can be put when running a blocking server (defaults to `NULL`). See the [*The event cycle in fiery* vignette](https://fiery.data-imaginist.com/articles/events.html) for more information.
    trigger_dir = function(dir) {
      if (missing(dir)) return(private$TRIGGERDIR)
      check_dir(dir, allow_null = TRUE)
      private$TRIGGERDIR <- dir
    },
    #' @field plugins A named list of the already attached plugins. **Read Only** - can only be modified using the `attach()` method.
    plugins = function(plugin) {
      if (!missing(plugin)) {
        cli::cli_abort('Use the {.fn attach} method to add plugins')
      }
      private$pluginList
    },
    #' @field root The location of the app. Setting this will remove the root value from requests (or decline them with `400` if the request does not match the root). E.g. the path of a request will be changed from `/demo/test` to `/test` if `root == '/demo'`
    root = function(path) {
      if (missing(path)) return(private$ROOT)
      check_string(path)
      path <- sub('/$', '', path)
      if (path != '') path <- paste0('/', sub('^/+', '', path))
      private$ROOT <- path
    },
    #' @field access_log_format A [glue][glue::glue] string defining how requests will be logged. For standard formats see [common_log_format] and [combined_log_format]. Defaults to the *Common Log Format*
    access_log_format = function(format) {
      if (missing(format)) return(private$ACCESS_LOG_FORMAT)
      check_string(format)
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
    logger = list(logger_null()),
    
    DELAY = NULL,
    TIME = NULL,
    ASYNC = NULL,
    LOG_QUEUE = NULL,
    
    # Methods
    run = function(block = TRUE, resume = FALSE, showcase = FALSE, ..., silent = FALSE) {
      check_bool(block)
      check_bool(resume)
      check_bool(showcase)
      check_bool(silent)
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
      if (req$SCRIPT_NAME != self$root && !grepl(paste0('^', self$root, '(/|$)'), req$PATH_INFO)) {
        cli::cli_abort('URL ({req$PATH_INFO}) not matching mount point ({self$root})')
      }
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
          check_bool(continue)
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
        for (val in res) if (is.error_cond(val)) self$log('error', paste0(conditionMessage(val), ' from ', deparse(conditionCall(val), nlines = 1)))
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
          self$log('error', paste0(conditionMessage(e), ' from ', deparse(conditionCall(e), nlines = 1)))
        }),
        warning = function(w) {
          self$log('warning', paste0(conditionMessage(w), ' from ', deparse(conditionCall(w), nlines = 1)))
          invokeRestart('muffleWarning')
        },
        message = function(m) {
          self$log('message', paste0(conditionMessage(m), ' from ', deparse(conditionCall(m), nlines = 1)))
          invokeRestart('muffleMessage')
        }
      )
    },
    send_ws = function(message, id) {
      if (!is.raw(message)) {
        check_string(message)
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
