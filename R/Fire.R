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
#' @importFrom utils browseURL
#' @importFrom later later
#' @importFrom stats setNames
#' @importFrom reqres Request
#' @importFrom stringi stri_pad_left
#' @importFrom sodium hex2bin
#' @importFrom promises is.promising
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
Fire <- R6Class(
  'Fire',
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
      private$handlers <- list()
      private$websockets <- list()
      private$client_id <- session_id_cookie()
      private$DELAY <- DelayStack(self)
      private$TIME <- TimeStack(self)
      private$ASYNC <- AsyncStack$new(self)
      private$LOG_QUEUE <- DelayStack()
      private$SESSION_NAME <- gsub(" ", "_", cli::hash_animal(runif(1))$hash)
      private$SESSION_FRAMEWORK_VERSION <- utils::packageVersion("fiery")
      self$set_logger(logger_void)
      private$ACCESS_LOG_FORMAT <- common_log_formatter
    },
    #' @description Human readable description of the app
    #' @param ... ignored
    #' @return A character vector
    format = function(...) {
      text <- c(
        '\U0001f525 A fiery webserver',
        '\U0001f525  \U0001f4a5   \U0001f4a5   \U0001f4a5'
      )
      mat <- matrix(
        c('Running on', ': ', paste0(self$host, ':', self$port, self$root)),
        ncol = 3
      )
      plugins <- names(private$pluginList)
      if (is.null(plugins)) {
        plugins <- 'none'
      }
      mat <- rbind(mat, c('Plugins attached', ': ', plugins[1]))
      mat <- rbind(
        mat,
        matrix(c(rep('  ', (length(plugins) - 1) * 2), plugins[-1]), ncol = 3)
      )
      handlers <- lapply(private$handlers, function(x) x$length())
      if (length(handlers) == 0) {
        mat <- rbind(mat, c('Event handlers added', ': ', 'none'))
      } else {
        mat <- rbind(mat, c('Event handlers added', '', ''))
        order <- match(names(handlers), private$privateTriggers)
        order[is.na(order)] <- seq_len(sum(is.na(order))) +
          max(order, na.rm = TRUE)
        handlers <- handlers[order(order)]
        mat <- rbind(
          mat,
          matrix(
            c(
              names(handlers),
              rep(': ', length(handlers)),
              as.character(unlist(handlers))
            ),
            ncol = 3
          )
        )
      }
      mat[, 1] <- stri_pad_left(mat[, 1], max(nchar(mat[, 1])))
      c(text, paste0('\U0001f525 ', apply(mat, 1, paste, collapse = '')))
    },
    #' @description Begin running the server. Will trigger the `start` event
    #' @param block Should the console be blocked while running (alternative is to run in the background)
    #' @param showcase Should the default browser open up at the server address.
    #' If `TRUE` then a browser opens at the root of the app. If a string the
    #' string is used as a path to add to the root before opening
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
          private$running <- FALSE
          private$p_trigger('end', server = self)
          stopServer(private$server)
          private$server <- NULL
          private$p_log('stop', paste0(self$host, ':', self$port, self$root))
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
    #' @param id An optional id to use to identify the handler
    #' @return A unique string identifying the handler (either `id` or generated for you)
    on = function(event, handler, pos = NULL, id = NULL) {
      check_string(event)
      check_function(handler)
      # We do this to ensure that any set of arguments is valid to the handler
      if (!"..." %in% fn_fmls_names(handler)) {
        fn_fmls(handler) <- c(fn_fmls(handler), "..." = missing_arg())
      }
      check_string(id, allow_null = TRUE)
      if (!is.null(id) && id %in% names(private$handlerMap)) {
        cli::cli_abort(
          "{.arg id} must be unique. A handler with this id has already been added"
        )
      }
      handlerId <- id %||% reqres::random_key()
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
        cli::cli_abort(
          '{.val {event}} and other protected events cannot be triggered manually'
        )
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
    #' @description Serve a file or directory of files at a specified url path. Requests matching a file on the system never enters into the request loop but are served directly (and fast). Due to this, logging for these requests are also turned off
    #' @param at The url path to listen to requests on
    #' @param path The path to the file or directory on the file system
    #' @param use_index Should an `index.html` file be served if present when a client requests the folder
    #' @param fallthrough Should requests that doesn't match a file enter the request loop or have a 404 response send directly
    #' @param html_charset The charset to report for serving html files
    #' @param headers A list of headers to add to the response. Will be combined with the global headers of the app
    #' @param validation An optional validation pattern. Presently, the only type of validation supported is an exact string match of a header. For example, if validation is `"abc" = "xyz"`, then HTTP requests must have a header named `abc` (case-insensitive) with the value `"xyz"` (case-sensitive). If a request does not have a matching header, than httpuv will give a 403 Forbidden response. If `character(0)` (the default), then no validation check will be performed.
    serve_static = function(
      at,
      path,
      use_index = TRUE,
      fallthrough = FALSE,
      html_charset = "utf-8",
      headers = list(),
      validation = NULL
    ) {
      check_string(at)
      check_string(path)
      if (!file.exists(path) && !dir.exists(path)) {
        cli::cli_abort(
          "{.arg {path}} does not point to an existing file or directory"
        )
      }
      check_bool(use_index)
      check_bool(fallthrough)
      check_string(html_charset)
      if (!(rlang::is_bare_list(headers) && rlang::is_named2(headers))) {
        stop_input_type(headers, "a named list")
      }
      for (i in names(headers)) {
        check_string(headers[[i]], arg = paste0("headers$", i))
      }
      check_string(validation, allow_null = TRUE)
      if (at %in% names(private$staticList)) {
        cli::cli_inform("Overwriting static url path {.field {at}}")
      }
      private$staticList[[at]] <- httpuv::staticPath(
        path = path,
        indexhtml = use_index,
        fallthrough = fallthrough,
        html_charset = html_charset,
        headers = headers,
        validation = validation %||% character(0L)
      )
      invisible(NULL)
    },
    #' @description Exclude a url path from serving static content. Only meaningful to exclude sub paths of path that are set to serve static content
    #' @param at The url path to exclude from static serving. Request to this path will enter the normal request loop
    exclude_static = function(at) {
      check_string(at)
      private$staticList[[at]] <- httpuv::excludeStaticPath()
      invisible(NULL)
    },
    #' @description Attach a plugin to the app. See the [*Creating and using fiery plugins* vignette](https://fiery.data-imaginist.com/articles/plugins.html) for more information
    #' @param plugin The plugin to attach
    #' @param name Optional name for the plugin. If omitted `plugin$name` will be used instead
    #' @param ... Arguments to pass into the plugins `on_attach()` method
    #' @param force If the plugin has already been attached an error is thrown, unless `force = TRUE` which tells the app to reattach it
    attach = function(plugin, ..., name = NULL, force = FALSE) {
      name <- name %||% plugin$name
      check_string(name)

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
          cli::cli_abort(
            'The {.arg {name}} plugin requires the following {cli::qty(requires[!exists])} plugin{?s}: {requires[!exists]}'
          )
        }
      }
      try_fetch(
        plugin$on_attach(self, ...),
        error = function(cnd) {
          cli::cli_abort(
            'The {.arg {name}} plugin failed to attach to the app',
            parent = cnd
          )
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
      if (missing(value)) {
        return(private$headers[[tolower(name)]])
      }
      check_string(value, allow_null = TRUE)
      private$headers[[tolower(name)]] <- value
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
    #' @description Add a timed evaluation that will be evaluated after the given number of seconds.
    #' @param expr The expression to evaluate when the time has passed
    #' @param then A handler to call once `expr` has been evaluated
    #' @param after The time in second to wait before evaluating `expr`
    #' @param loop Should `expr` be called repeatedly with the interval given by `after`
    #' @return A unique id identifying the handler
    time = function(expr, then, after, loop = FALSE) {
      private$TIME$add({{ expr }}, then, after, loop)
    },
    #' @description Remove a timed evaluation
    #' @param id The unique id identifying the handler
    remove_time = function(id) {
      private$TIME$remove(id)
    },
    #' @description Add a delayed evaluation to be evaluated immediately at the end of the loop cycle.
    #' @param expr The expression to evaluate at the end of the cycle
    #' @param then A handler to call once `expr` has been evaluated
    #' @return A unique id identifying the handler
    delay = function(expr, then) {
      private$DELAY$add({{ expr }}, then)
    },
    #' @description Remove a delayed evaluation
    #' @param id The unique id identifying the handler
    remove_delay = function(id) {
      private$DELAY$remove(id)
    },
    #' @description `r lifecycle::badge('deprecated')` Add an asynchronous evaluation to be evaluated in another process without blocking the server. This function has been deprecated in favor of using your own async framework of choice, e.g. [mirai](https://mirai.r-lib.org) or [promises](https://rstudio.github.io/promises/)
    #' @param expr The expression to evaluate at the end of the cycle
    #' @param then A handler to call once `expr` has been evaluated
    #' @return A unique id identifying the handler
    async = function(expr, then) {
      lifecycle::deprecate_soft(
        "2.0.0",
        what = "Fire$async()",
        details = "Use an async evaluation framework of your own choice, such as promises or mirai"
      )
      private$ASYNC$add(substitute(expr), then)
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
      if (identical(logger, logger_void)) {
        private$logger <- NULL
      } else  {
        check_args(logger, c('event', 'message', 'request', '...'))
        private$logger <- logger
      }
      invisible(NULL)
    },
    #' @description Log a message with the logger attached to the app. See [loggers] for build in functionality
    #' @param event The event associated with the message
    #' @param message The message to log
    #' @param request The `Request` object associated with the message, if any.
    #' @param ... Additional arguments passed on to the logger.
    #' @param .logcall The call that send the log request
    #' @param .topcall The call in which `.logcall` is called from
    #' @param .topenv The environment associated with `.topcall`
    log = function(
      event,
      message,
      request = NULL,
      ...,
      .logcall = sys.call(),
      .topcall = sys.call(-1),
      .topenv = parent.frame()
    ) {
      if (is.null(private$logger)) {
        return(invisible(NULL))
      }
      time <- Sys.time()
      force(message)
      force(.logcall)
      force(.topcall)
      force(.topenv)
      log_fun <- function(...) {
        private$logger(
          event,
          message,
          request,
          time,
          .logcall = .logcall,
          .topcall = .topcall,
          .topenv = .topenv,
          .session_name = private$SESSION_NAME,
          ...
        )
      }
      if (private$running) {
        private$LOG_QUEUE$add(
          NULL,
          log_fun
        )
      } else {
        log_fun()
      }
      invisible(NULL)
    },
    #' @description Test if an app is running
    is_running = function() {
      private$running
    },
    #' @description Evaluate an expression safely, logging any errors, warnings,
    #' or messages that bubbles up
    #' @param expr An expression to evaluate
    #' @param request The request under evaluation, if any. Used in logging
    #'
    #' @return The value of the expression. If an error is caught, the condition
    #' object is returned instead
    #'
    safe_call = function(expr, request = NULL) {
      private$p_safe_call(expr, request = request)
    },
    #' @description Send a request directly to the request logic of a non-running app. Only intended for testing the request logic
    #' @param request The request to send
    test_request = function(request) {
      res <- private$header_logic(request)
      res %||% private$request_logic(request)
    },
    #' @description Send a request directly to the header logic of a non-running app. Only intended for testing the request logic
    #' @param request The request to send
    test_header = function(request) {
      on.exit(
        if (!is.null(request$._REQRES_OBJ)) put_request(request$._REQRES_OBJ)
      )
      private$header_logic(request)
    },
    #' @description Send a message directly **to** the message logic of a non-running app. Only intended for testing the websocket logic
    #' @param request The request to use to establish the connection
    #' @param binary Is the message send in binary or character format
    #' @param message The message to send. If `binary = FALSE` a character vector, if `binary = TRUE` a raw vector
    #' @param withClose Should the websocket connection be closed at the end by the client
    test_message = function(request, binary, message, withClose = TRUE) {
      req <- private$new_req(request)
      id <- private$client_id(req)
      message_fun <- private$message_logic(id, req)
      message_fun(binary, message)
      if (withClose) {
        close_fun <- private$close_ws_logic(id, req)
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
        send = function(message) {
          message(message)
        },
        close = function() {
          message('closing')
        }
      )
      private$websocket_logic(ws)
      self$send(message, private$client_id(private$new_req(request)))
      if (close) private$close_ws(private$client_id(private$new_req(request)))
    }
  ),
  active = list(
    #' @field host A string giving a valid IPv4 address owned by the server, or `'0.0.0.0'` to listen on all addresses. The default is `'127.0.0.1'`
    host = function(address) {
      if (missing(address)) {
        return(private$HOST)
      }
      check_string(address)
      private$HOST <- address
    },
    #' @field port An integer giving the port number the server should listen on (defaults to `8080L`)
    port = function(n) {
      if (missing(n)) {
        return(private$PORT)
      }
      check_number_whole(n, min = 1)
      private$PORT <- n
    },
    #' @field refresh_rate `r lifecycle::badge('deprecated')` The interval in seconds between run cycles when running a blocking server (defaults to `0.001`)
    refresh_rate = function(rate) {
      lifecycle::deprecate_soft(
        "1.5.0",
        I("Refresh rate for blocking servers"),
        details = "The loop now runs without any pause"
      )
      if (missing(rate)) {
        return(private$REFRESHRATE)
      }
      check_number_decimal(rate)
      private$REFRESHRATE <- rate
    },
    #' @field refresh_rate_nb The interval in seconds between run cycles when running a non-blocking server (defaults to `1`)
    refresh_rate_nb = function(rate) {
      if (missing(rate)) {
        return(private$REFRESHRATENB)
      }
      check_number_decimal(rate)
      private$REFRESHRATENB <- rate
    },
    #' @field trigger_dir A valid folder where trigger files can be put when running a blocking server (defaults to `NULL`). See the [*The event cycle in fiery* vignette](https://fiery.data-imaginist.com/articles/events.html) for more information.
    trigger_dir = function(dir) {
      if (missing(dir)) {
        return(private$TRIGGERDIR)
      }
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
    #' @field data_store Access the environment that holds the global data store
    data_store = function(value) {
      if (missing(value)) {
        return(private$data)
      }
      if (!identical(private$data, value)) {
        cli::cli_abort("It is not allowed to replace the data store")
      }
      private$data <- value
    },
    #' @field root The location of the app. Setting this will remove the root value from requests (or decline them with `400` if the request does not match the root). E.g. the path of a request will be changed from `/demo/test` to `/test` if `root == '/demo'`
    root = function(path) {
      if (missing(path)) {
        return(private$ROOT)
      }
      check_string(path)
      path <- sub('/$', '', path)
      if (path != '') {
        path <- paste0('/', sub('^/+', '', path))
      }
      private$ROOT <- path
    },
    #' @field access_log_format A [glue][glue::glue] string defining how requests will be logged. For standard formats see [common_log_format] and [combined_log_format]. Defaults to the *Common Log Format*
    access_log_format = function(format) {
      if (missing(format)) {
        return(private$ACCESS_LOG_FORMAT)
      }
      check_string(format)
      if (format == common_log_format) {
        format <- common_log_formatter
      } else if (format == combined_log_format) {
        format <- combined_log_formatter
      }
      private$ACCESS_LOG_FORMAT <- format
    },
    #' @field key The encryption key to use for request/response encryption
    #'
    key = function(value) {
      if (missing(value)) {
        cli::cli_abort("{.arg key} can only be set, not retrieved")
      }
      if (!is.null(value)) {
        if (is_string(value)) {
          value <- hex2bin(value)
          if (length(value) == 0) {
            cli::cli_abort(
              "Malformed key. If given as a string it must be hexadecimal encoded"
            )
          }
        }
        if (!is.raw(value)) {
          cli::cli_abort(
            "Malformed key. It must be provided as either a string, a raw vector or NULL"
          )
        }
        if (length(value) != 32) {
          cli::cli_abort("Malformed key. The key must be 32 bit")
        }
      }
      private$KEY <- value
    },
    #' @field session_cookie_settings Get or set the session cookie settings
    #'
    session_cookie_settings = function(value) {
      if (missing(value)) {
        return(private$SESSION_COOKIE)
      }
      if (!reqres::is_session_cookie_settings(value)) {
        cli::cli_abort(c(
          "{.arg session_cookie_settings} can only be set to a valid settings object",
          "i" = "Construct one using {.fun reqres::session_cookie}"
        ))
      }
      private$SESSION_COOKIE <- value
    },
    #' @field trust A logical indicating whether incoming requests are trusted.
    #'
    trust = function(value) {
      if (missing(value)) {
        return(private$TRUST)
      }
      check_bool(value)
      private$TRUST <- value
    },
    #' @field compression_limit The size threshold in bytes for trying to
    #' compress the response body (it is still dependant on content negotiation)
    compression_limit = function(value) {
      if (missing(value)) {
        return(private$COMPRESSION_LIMIT)
      }
      check_number_decimal(value, min = 0, allow_infinite = TRUE)
      private$COMPRESSION_LIMIT <- value
    },
    #' @field query_delim The delimeter used to split array-type query arguments
    #' when parsing the query string
    query_delim = function(value) {
      if (missing(value)) {
        return(private$QUERY_DELIM)
      }
      check_string(value, allow_null = TRUE)
      private$QUERY_DELIM <- value
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
    ACCESS_LOG_FORMAT = NULL,
    KEY = NULL,
    SESSION_COOKIE = NULL,
    TRUST = FALSE,
    COMPRESSION_LIMIT = 0,
    QUERY_DELIM = NULL,
    SESSION_NAME = "",
    SESSION_FRAMEWORK = "fiery",
    SESSION_FRAMEWORK_VERSION = "",

    running = FALSE,
    quitting = FALSE,
    privateTriggers = c(
      'start',
      'resume',
      'cycle-start',
      'header',
      'before-request',
      'request',
      'after-request',
      'before-message',
      'message',
      'after-message',
      'websocket-opened',
      'websocket-closed',
      'send',
      'cycle-end',
      'end'
    ),
    data = NULL,
    headers = list(),
    handlers = NULL,
    handlerMap = list(),
    pluginList = list(),
    staticList = list(),
    websockets = NULL,
    server = NULL,
    client_id = NULL,
    logger = NULL,

    DELAY = NULL,
    TIME = NULL,
    ASYNC = NULL,
    LOG_QUEUE = NULL,

    # Methods
    run = function(
      block = TRUE,
      resume = FALSE,
      showcase = FALSE,
      ...,
      silent = FALSE
    ) {
      check_bool(block)
      check_bool(resume)
      if (!is_bool(showcase)) {
        check_string(showcase, what = "`TRUE` or `FALSE` or a single string")
      }
      check_bool(silent)
      if (!private$running) {
        private$running <- TRUE
        private$TIME$reset()
        private$p_trigger('start', server = self, ...)
        if (resume) {
          private$p_trigger('resume', server = self, ...)
          if (!silent) {
            cli::cli_inform(
              'Fire restarted at {.url {self$host}:{self$port}{self$root}} ({private$SESSION_NAME})'
            )
          }
          private$p_log('resume', paste0(self$host, ':', self$port, self$root))
        } else {
          if (!silent) {
            cli::cli_inform(
              'Fire started at {.url {self$host}:{self$port}{self$root}} ({private$SESSION_NAME})'
            )
          }
          private$p_log('start', paste0(self$host, ':', self$port, self$root))
        }

        if (block) {
          on.exit({
            private$running <- FALSE
            private$p_trigger('end', server = self)
            private$p_log('stop', paste0(self$host, ':', self$port, self$root))
          })
          private$run_blocking_server(showcase = showcase)
        } else {
          private$run_allowing_server(showcase = showcase)
        }
      } else {
        private$p_log(
          'warning',
          'Server is already running and cannot be started'
        )
      }
    },
    run_blocking_server = function(showcase = FALSE) {
      server <- startServer(
        self$host,
        self$port,
        list(
          call = private$request_logic,
          onHeaders = private$header_logic,
          onWSOpen = private$websocket_logic,
          staticPaths = lapply(private$staticList, function(p) {
            attr(p, "headers") <- c(attr(p, "headers"), private$headers)
            p
          })
        )
      )

      on.exit(stopServer(server))

      if (!isFALSE(showcase)) {
        private$open_browser(if (is.character(showcase)) showcase else "")
      }

      repeat {
        private$p_trigger('cycle-start', server = self)
        service()
        private$external_triggers()
        private$DELAY$eval(server = self)
        private$TIME$eval(server = self)
        private$ASYNC$eval(server = self)
        private$LOG_QUEUE$eval(server = self)
        private$p_trigger('cycle-end', server = self)
        if (private$quitting) {
          private$quitting <- FALSE
          break
        }
      }
    },
    run_allowing_server = function(showcase = FALSE) {
      private$server <- startServer(
        self$host,
        self$port,
        list(
          call = private$request_logic,
          onHeaders = private$header_logic,
          onWSOpen = private$websocket_logic,
          staticPaths = lapply(private$staticList, function(p) {
            attr(p, "headers") <- c(attr(p, "headers"), private$headers)
            p
          })
        )
      )

      if (!isFALSE(showcase)) {
        private$open_browser(if (is.character(showcase)) showcase else "")
      }

      private$allowing_cycle()
    },
    allowing_cycle = function() {
      if (private$running) {
        private$p_trigger('cycle-start', server = self)
        private$external_triggers()
        private$DELAY$eval(server = self)
        private$TIME$eval(server = self)
        private$ASYNC$eval(server = self)
        private$LOG_QUEUE$eval(server = self)
        private$p_trigger('cycle-end', server = self)
        later(
          function() {
            private$allowing_cycle()
          },
          private$REFRESHRATENB
        )
      }
    },
    mount_request = function(req) {
      if (req$SCRIPT_NAME == self$root) {
        req
      } else if (grepl(paste0('^', self$root, '(/|$)'), req$PATH_INFO)) {
        req$SCRIPT_NAME <- self$root
        req$PATH_INFO <- sub(paste0('^', self$root, ''), '', req$PATH_INFO)
        req
      } else {
        cli::cli_abort(
          'URL ({req$PATH_INFO}) not matching mount point ({self$root})'
        )
      }
    },
    request_logic = function(req) {
      req <- req$._REQRES_OBJ
      start_time <- req$start_time %||% Sys.time()
      id <- private$client_id(req)
      args <- unlist(
        unname(private$p_trigger(
          'before-request',
          server = self,
          id = id,
          request = req,
          .request = req
        )),
        recursive = FALSE
      )
      res <- private$p_trigger(
        'request',
        server = self,
        id = id,
        request = req,
        arg_list = args,
        .request = req
      )
      any_promising <- any(vapply(res, promises::is.promising, logical(1)))
      if (any_promising) {
        promises::then(promises::promise_map(res, identity), function(res) {
          private$finish_request(res, req, start_time, id)
        })
      } else {
        private$finish_request(res, req, start_time, id)
      }
    },
    finish_request = function(res, request, start_time, id) {
      on.exit({
        request$locked <- FALSE
        put_request(request)
      })

      response <- request$respond()
      problems <- vapply(res, reqres::is_reqres_problem, logical(1))
      if (any(problems)) {
        reqres::handle_problem(response, res[[which(problems)[1]]])
      } else if (any(vapply(res, is_condition, logical(1)))) {
        response$status_with_text(500L)
      }
      response <- private$p_safe_call(response$as_list(), request)
      # On the off-chance that reqres throws an error during conversion of response
      if (is_condition(response)) {
        request$response$status_with_text(500L) # Update the real response first so it gets logged correctly
        response <- list(
          status = 500L,
          headers = list("Content-Type" = "text/plain"),
          body = "Internal Server Error"
        )
      }
      private$p_trigger(
        'after-request',
        server = self,
        id = id,
        request = request,
        response = request$response,
        .request = request
      )
      private$log_request(start_time, request, id)
      response
    },
    log_request = function(start_time, req, id) {
      end_time <- Sys.time()
      log_format <- private$ACCESS_LOG_FORMAT
      if (is.character(log_format)) {
        private$p_log(
          'request',
          glue_log(
            list(
              start_time = start_time,
              end_time = end_time,
              request = req,
              response = req$response,
              id = id
            ),
            log_format
          ),
          req
        )
      } else {
        res <- req$response
        private$p_log(
          "request",
          log_format(
            ip = req$ip,
            id = id,
            end_time = format(end_time, "%d/%b/%Y:%T %z"),
            method = toupper(req$method),
            path = req$path,
            querystring = req$querystring,
            protocol = toupper(req$protocol),
            status = res$status,
            content_length = res$content_length(),
            referer = req$get_header("Referer") %||% "",
            user_agent = req$get_header("User-Agent") %||% ""
          ),
          req
        )
      }
    },
    header_logic = function(req) {
      start_time <- Sys.time()
      request <- private$p_safe_call(
        private$mount_request(req),
        private$new_req(req, otel = FALSE)
      )
      response <- NULL
      if (is_condition(request)) {
        req <- private$new_req(req, auto_put = FALSE)
        id <- private$client_id(req)
        response <- req$respond()
        response$status_with_text(400L)
        response <- private$p_safe_call(response$as_list(), req)
        # On the off-chance that reqres throws an error during conversion of response
        if (is_condition(response)) {
          req$response$status_with_text(500L) # Update the real response first so it gets logged correctly
          response <- list(
            status = 500L,
            headers = list("Content-Type" = "text/plain"),
            body = "Internal Server Error"
          )
        }
      } else {
        req <- private$new_req(request, auto_put = FALSE)
        request$._REQRES_OBJ <- req
        # Short-circuit if no header handlers exist
        if (!is.null(private$handlers[["header"]])) {
          id <- private$client_id(req)
          res <- private$p_trigger(
            'header',
            server = self,
            id = id,
            request = req,
            .request = req
          )
          problems <- vapply(res, reqres::is_reqres_problem, logical(1))
          response <- req$respond()
          if (any(problems)) {
            reqres::handle_problem(req$respond(), res[[which(problems)[1]]])
            res <- FALSE
          } else if (any(vapply(res, is_condition, logical(1)))) {
            response$status_with_text(500L)
            res <- FALSE
          }
          if (length(res) == 0) {
            res <- NULL
          } else {
            continue <- tail(res, 1)[[1]]
            check_bool(continue)
            if (continue) {
              response <- NULL
            } else {
              private$p_log('request', 'denied after header', req)
              response <- private$p_safe_call(req$respond()$as_list(), req)
              if (is_condition(response)) {
                req$response$status_with_text(500L)
                response <- list(
                  status = 500L,
                  headers = list("Content-Type" = "text/plain"),
                  body = "Internal Server Error"
                )
              }
            }
          }
        }
      }
      if (!is.null(response)) {
        private$log_request(req$start_time %||% start_time, req, id)
        on.exit(put_request(req))
      }
      response
    },
    websocket_logic = function(ws) {
      request <- private$p_safe_call(
        private$mount_request(ws$request),
        private$new_req(ws$request, otel = FALSE)
      )
      if (is_condition(request)) {
        ws$close()
        return()
      } else {
        req <- private$new_req(request, otel = FALSE)
      }
      id <- private$client_id(req)
      private$websockets[[id]] <- ws
      private$p_log('websocket', paste0('connection established to ', id), req)
      private$p_trigger(
        'websocket-opened',
        server = self,
        id = id,
        connection = ws,
        .request = request
      )
      ws$onMessage(private$message_logic(id, req))
      ws$onClose(private$close_ws_logic(id, req))
    },
    message_logic = function(id, request) {
      force(id)
      force(request)
      function(binary, msg) {
        start <- Sys.time()
        args <- unlist(
          unname(
            private$p_trigger(
              'before-message',
              server = self,
              id = id,
              binary = binary,
              message = msg,
              request = request,
              .request = request
            )
          ),
          recursive = FALSE
        )
        if (is.null(args)) {
          args <- structure(list(), names = character())
        }
        if ('binary' %in% names(args)) {
          binary <- args$binary
        }
        if ('message' %in% names(args)) {
          msg <- args$message
        }
        args <- modifyList(args, list(binary = NULL, message = NULL))

        private$p_trigger(
          'message',
          server = self,
          id = id,
          binary = binary,
          message = msg,
          request = request,
          arg_list = args,
          .request = request
        )

        private$p_trigger(
          'after-message',
          server = self,
          id = id,
          binary = binary,
          message = msg,
          request = request,
          .request = request
        )

        private$p_log(
          'websocket',
          paste0(
            'from ',
            id,
            ' processed in ',
            format(Sys.time() - start, digits = 3)
          ),
          request,
          ws_message = msg
        )
      }
    },
    close_ws_logic = function(id, request) {
      force(id)
      force(request)
      function() {
        private$p_trigger(
          'websocket-closed',
          server = self,
          id = id,
          request = request,
          .request = request
        )
        private$p_log(
          'websocket',
          paste0('connection to ', id, ' closed from the client'),
          request
        )
      }
    },
    add_handler = function(event, handler, pos, id) {
      if (is.null(private$handlers[[event]])) {
        private$handlers[[event]] <- HandlerStack(server = self)
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
    p_trigger = function(event, ..., .request = NULL) {
      if (!is.null(private$handlers[[event]])) {
        res <- private$handlers[[event]]$dispatch(..., .request = .request)
        res <- lapply(res, function(r) {
          if (promises::is.promising(r)) {
            if (!is.null(.request)) {
              .request$locked <- TRUE
            }
            promises::catch(r, function(r) {
              private$p_safe_call(cnd_signal(r), .request)
            })
          } else {
            r
          }
        })
      } else {
        res <- set_names(list())
      }
      res
    },
    p_safe_call = function(expr, request = NULL) {
      try_fetch(
        expr,
        error = function(e) {
          topcall <- e$call
          if (MAY_TRACEBACK && !is.null(topcall)) {
            bt <- e$trace %||% cheap_trace_back()
            topcall_pos <- which(bt$call == topcall)
            if (length(topcall_pos) != 1 || !any(bt$parent == topcall_pos)) {
              private$p_log('error', e)
            } else {
              private$p_log(
                'error',
                e,
                request = request,
                .logcall = bt$call[[which(bt$parent == topcall_pos)[1]]],
                .topcall = topcall,
                .topenv = sys.frame(topcall_pos)
              )
            }
          } else {
            private$p_log('error', e)
          }
          add_otel_exception_event(e)
        },
        warning = function(w) {
          topcall <- w$call
          if (MAY_TRACEBACK && !is.null(topcall)) {
            bt <- cheap_trace_back()
            topcall_pos <- which(bt$call == topcall)
            if (length(topcall_pos) != 1 || !any(bt$parent == topcall_pos)) {
              private$p_log('warning', w)
            } else {
              private$p_log(
                'warning',
                w,
                request = request,
                .logcall = bt$call[[which(bt$parent == topcall_pos)[1]]],
                .topcall = topcall,
                .topenv = sys.frame(topcall_pos)
              )
            }
          } else {
            private$p_log("warning", w)
          }
          cnd_muffle(w)
        },
        message = function(m) {
          private$p_log("message", m)
          cnd_muffle(m)
        }
      )
    },
    p_log = function(
      event,
      message,
      request = NULL,
      ...,
      .logcall = sys.call(),
      .topcall = sys.call(-1),
      .topenv = parent.frame()
    ) {
      if (is.null(private$logger)) {
        return(invisible(NULL))
      }
      time <- Sys.time()
      force(message)
      force(.logcall)
      force(.topcall)
      force(.topenv)
      log_fun <- function(...) {
        private$logger(
          event,
          message,
          request,
          time,
          .logcall = .logcall,
          .topcall = .topcall,
          .topenv = .topenv,
          .session_name = private$SESSION_NAME,
          ...
        )
      }
      if (private$running) {
        private$LOG_QUEUE$add(
          NULL,
          log_fun
        )
      } else {
        log_fun()
      }
      invisible(NULL)
    },
    external_triggers = function() {
      if (is.null(private$TRIGGERDIR)) {
        return()
      }

      triggerFiles <- list.files(
        private$TRIGGERDIR,
        pattern = '*.rds',
        ignore.case = TRUE,
        full.names = TRUE
      )
      while (length(triggerFiles) > 0) {
        nextFile <- order(file.info(triggerFiles)$ctime)[1]
        event <- sub(
          '\\.rds$',
          '',
          basename(triggerFiles[nextFile]),
          ignore.case = TRUE
        )
        args <- readRDS(triggerFiles[nextFile])
        unlink(triggerFiles[nextFile])
        if (!is.list(args)) {
          private$p_log(
            'warning',
            'External triggers must be an rds file containing a list'
          )
        } else {
          args$event <- event
          args$server <- self
          inject(private$p_trigger(!!!args))
        }
        triggerFiles <- list.files(
          private$TRIGGERDIR,
          pattern = '*.rds',
          ignore.case = TRUE,
          full.names = TRUE
        )
      }
    },
    send_ws = function(message, id) {
      if (!is.raw(message)) {
        check_string(message)
      }
      if (missing(id) || is.null(id)) {
        id <- names(private$websockets)
      } else {
        id <- intersect(id, names(private$websockets))
      }
      if (length(id) == 0) {
        return(NULL)
      }
      for (i in id) {
        private$websockets[[i]]$send(message)
      }
      private$p_log('websocket', paste0('send to ', paste(id, collapse = ', ')))
    },
    close_ws = function(id) {
      ws <- private$websockets[[id]]
      if (!is.null(ws)) {
        try(ws$close(), silent = TRUE)
        private$websockets[[id]] <- NULL
        private$p_log(
          'websocket',
          paste0('connection to ', id, ' closed from the server')
        )
      }
    },
    open_browser = function(path = "") {
      url <- paste0(
        'http://',
        private$HOST,
        ':',
        private$PORT,
        '/',
        sub("^/", "", path)
      )
      browseURL(url)
    },
    new_req = function(request, otel = TRUE, auto_put = TRUE) {
      req <- get_request(
        rook = request,
        trust = private$TRUST,
        key = private$KEY,
        session_cookie = private$SESSION_COOKIE,
        compression_limit = private$COMPRESSION_LIMIT,
        query_delim = private$QUERY_DELIM,
        response_headers = private$headers,
        with_otel = otel
      )
      if (!is.null(req$otel)) {
        req$otel$set_attribute("server.id", private$SESSION_NAME)
        req$otel$set_attribute(
          "server.framework.name",
          private$SESSION_FRAMEWORK
        )
        req$otel$set_attribute(
          "server.framework.version",
          private$SESSION_FRAMEWORK_VERSION
        )
        req$otel$set_attribute("network.local.address", private$HOST)
        req$otel$set_attribute("network.local.port", private$PORT)
      }
      if (auto_put) {
        f <- as.call(list(function() put_request(req)))
        envir <- parent.frame()
        do.call(on.exit, list(f, TRUE, TRUE), envir = envir)
      }
      req
    },
    finalize = function() {
      if (private$running) {
        self$extinguish()
      }
    }
  )
)

# For use by connect etc
launch_server <- function(settings, host = NULL, port = NULL, ...) {
  server_yml <- yaml::read_yaml(settings)

  if (!is.null(server_yml$options)) {
    options(server_yml$options)
  }

  if (is.null(server_yml$constructor)) {
    cli::cli_abort(
      "The {.field constructor} field in {.file {settings}} must be present"
    )
  }
  constructor <- fs::path(
    fs::path_dir(settings),
    server_yml$constructor
  )
  if (!fs::file_exists(constructor)) {
    cli::cli_abort(
      "The {.field constructor} field in {.file {settings}} must point to an existing file"
    )
  }
  app <- source(constructor, verbose = FALSE)
  if (!inherits(app, "Fire")) {
    cli::cli_abort(
      "The constructor file in {.file {settings}} did not produce a Fire app"
    )
  }

  if (!is.null(host)) {
    app$host <- host
  }
  if (!is.null(port)) {
    app$port <- port
  }
  app$ignite()
}
