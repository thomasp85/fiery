# Generate a New App Object

The Fire generator creates a new `Fire`-object, which is the class
containing all the app logic. The class is based on the
[R6](https://r6.r-lib.org/reference/R6Class.html) OO-system and is thus
reference-based with methods and data attached to each object, in
contrast to the more well known S3 and S4 systems. A `fiery` server is
event driven, which means that it is build up and manipulated by adding
event handlers and triggering events. To learn more about the `fiery`
event model, read the [event
vignette](https://fiery.data-imaginist.com/articles/events.html).
`fiery` servers can be modified directly or by attaching plugins. As
with events, [plugins has its own
vignette](https://fiery.data-imaginist.com/articles/plugins.html).

### Initialization

A new 'Fire'-object is initialized using the `new()` method on the
generator:

|                                                     |
|-----------------------------------------------------|
| `app <- Fire$new(host = '127.0.0.1', port = 8080L)` |

### Copying

As `Fire` objects are using reference semantics new copies of an app
cannot be made simply be assigning it to a new variable. If a true copy
of a `Fire` object is desired, use the `clone()` method.

## Active bindings

- `host`:

  A string giving a valid IPv4 address owned by the server, or
  `'0.0.0.0'` to listen on all addresses. The default is `'127.0.0.1'`

- `port`:

  An integer giving the port number the server should listen on
  (defaults to `8080L`)

- `refresh_rate`:

  The interval in seconds between run cycles when running a blocking
  server (defaults to `0.001`)

- `refresh_rate_nb`:

  The interval in seconds between run cycles when running a non-blocking
  server (defaults to `1`)

- `trigger_dir`:

  A valid folder where trigger files can be put when running a blocking
  server (defaults to `NULL`). See the [*The event cycle in fiery*
  vignette](https://fiery.data-imaginist.com/articles/events.html) for
  more information.

- `plugins`:

  A named list of the already attached plugins. **Read Only** - can only
  be modified using the [`attach()`](https://rdrr.io/r/base/attach.html)
  method.

- `data_store`:

  Access the environment that holds the global data store

- `root`:

  The location of the app. Setting this will remove the root value from
  requests (or decline them with `400` if the request does not match the
  root). E.g. the path of a request will be changed from `/demo/test` to
  `/test` if `root == '/demo'`

- `access_log_format`:

  A [glue](https://glue.tidyverse.org/reference/glue.html) string
  defining how requests will be logged. For standard formats see
  [common_log_format](https://fiery.data-imaginist.com/reference/loggers.md)
  and
  [combined_log_format](https://fiery.data-imaginist.com/reference/loggers.md).
  Defaults to the *Common Log Format*

- `key`:

  The encryption key to use for request/response encryption

- `session_cookie_settings`:

  Get or set the session cookie settings

- `trust`:

  A logical indicating whether incoming requests are trusted.

- `compression_limit`:

  The size threshold in bytes for trying to compress the response body
  (it is still dependant on content negotiation)

- `query_delim`:

  The delimeter used to split array-type query arguments when parsing
  the query string

## Methods

### Public methods

- [`Fire$new()`](#method-Fire-new)

- [`Fire$format()`](#method-Fire-format)

- [`Fire$ignite()`](#method-Fire-ignite)

- [`Fire$start()`](#method-Fire-start)

- [`Fire$reignite()`](#method-Fire-reignite)

- [`Fire$resume()`](#method-Fire-resume)

- [`Fire$extinguish()`](#method-Fire-extinguish)

- [`Fire$stop()`](#method-Fire-stop)

- [`Fire$on()`](#method-Fire-on)

- [`Fire$off()`](#method-Fire-off)

- [`Fire$trigger()`](#method-Fire-trigger)

- [`Fire$send()`](#method-Fire-send)

- [`Fire$close_ws_con()`](#method-Fire-close_ws_con)

- [`Fire$serve_static()`](#method-Fire-serve_static)

- [`Fire$exclude_static()`](#method-Fire-exclude_static)

- [`Fire$attach()`](#method-Fire-attach)

- [`Fire$has_plugin()`](#method-Fire-has_plugin)

- [`Fire$header()`](#method-Fire-header)

- [`Fire$set_data()`](#method-Fire-set_data)

- [`Fire$get_data()`](#method-Fire-get_data)

- [`Fire$remove_data()`](#method-Fire-remove_data)

- [`Fire$time()`](#method-Fire-time)

- [`Fire$remove_time()`](#method-Fire-remove_time)

- [`Fire$delay()`](#method-Fire-delay)

- [`Fire$remove_delay()`](#method-Fire-remove_delay)

- [`Fire$async()`](#method-Fire-async)

- [`Fire$remove_async()`](#method-Fire-remove_async)

- [`Fire$set_client_id_converter()`](#method-Fire-set_client_id_converter)

- [`Fire$set_logger()`](#method-Fire-set_logger)

- [`Fire$log()`](#method-Fire-log)

- [`Fire$is_running()`](#method-Fire-is_running)

- [`Fire$safe_call()`](#method-Fire-safe_call)

- [`Fire$test_request()`](#method-Fire-test_request)

- [`Fire$test_header()`](#method-Fire-test_header)

- [`Fire$test_message()`](#method-Fire-test_message)

- [`Fire$test_websocket()`](#method-Fire-test_websocket)

- [`Fire$clone()`](#method-Fire-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `Fire` app

#### Usage

    Fire$new(host = "127.0.0.1", port = 8080)

#### Arguments

- `host`:

  A string overriding the default host

- `port`:

  An port number overriding the default port

#### Returns

A `Fire` object

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Human readable description of the app

#### Usage

    Fire$format(...)

#### Arguments

- `...`:

  ignored

#### Returns

A character vector

------------------------------------------------------------------------

### Method `ignite()`

Begin running the server. Will trigger the `start` event

#### Usage

    Fire$ignite(block = TRUE, showcase = FALSE, ..., silent = FALSE)

#### Arguments

- `block`:

  Should the console be blocked while running (alternative is to run in
  the background)

- `showcase`:

  Should the default browser open up at the server address. If `TRUE`
  then a browser opens at the root of the app. If a string the string is
  used as a path to add to the root before opening

- `...`:

  Arguments passed on to the `start` handler

- `silent`:

  Should startup messaging by silenced

------------------------------------------------------------------------

### Method [`start()`](https://rdrr.io/r/stats/start.html)

Synonymous method to `ignite()`

#### Usage

    Fire$start(...)

#### Arguments

- `...`:

  passed on to `ignite()`

------------------------------------------------------------------------

### Method `reignite()`

Resume a session. This is equivalent to `ignite()` but will also trigger
the `resume` event

#### Usage

    Fire$reignite(...)

#### Arguments

- `...`:

  passed on to `ignite()`

------------------------------------------------------------------------

### Method `resume()`

Synonymous method to `reignite()`

#### Usage

    Fire$resume(...)

#### Arguments

- `...`:

  passed on to `ignite()`

------------------------------------------------------------------------

### Method `extinguish()`

Stop the server. Will trigger the `end` event

#### Usage

    Fire$extinguish()

------------------------------------------------------------------------

### Method [`stop()`](https://rdrr.io/r/base/stop.html)

Synonymous method to `extinguish()`

#### Usage

    Fire$stop()

------------------------------------------------------------------------

### Method `on()`

Add a handler to an event. See the [*The event cycle in fiery*
vignette](https://fiery.data-imaginist.com/articles/events.html) for
more information.

#### Usage

    Fire$on(event, handler, pos = NULL, id = NULL)

#### Arguments

- `event`:

  The name of the event that should trigger the handler

- `handler`:

  The handler function that should be triggered

- `pos`:

  The position in the handler stack to place it at. `NULL` will place it
  at the end.

- `id`:

  An optional id to use to identify the handler

#### Returns

A unique string identifying the handler (either `id` or generated for
you)

------------------------------------------------------------------------

### Method `off()`

Remove an event handler from the app.

#### Usage

    Fire$off(handlerId)

#### Arguments

- `handlerId`:

  The unique id identifying the handler

------------------------------------------------------------------------

### Method `trigger()`

Trigger an event in the app. This will cause any handler attached to the
event to be called. See the [*The event cycle in fiery*
vignette](https://fiery.data-imaginist.com/articles/events.html) for
more information.

#### Usage

    Fire$trigger(event, ...)

#### Arguments

- `event`:

  The name of the event

- `...`:

  Arguments passed on to the handlers

#### Returns

A named list containing the return values of all handlers attached to
the event

------------------------------------------------------------------------

### Method `send()`

Send a Websocket message to a client. Will trigger the `send` event.

#### Usage

    Fire$send(message, id)

#### Arguments

- `message`:

  The message to send

- `id`:

  The id of the client to send to. If missing, the message will be send
  to all clients

------------------------------------------------------------------------

### Method `close_ws_con()`

Close a Websocket connection. Will trigger the `websocket-closed` event

#### Usage

    Fire$close_ws_con(id)

#### Arguments

- `id`:

  The id of the client to close the websocket connection to

------------------------------------------------------------------------

### Method `serve_static()`

Serve a file or directory of files at a specified url path. Requests
matching a file on the system never enters into the request loop but are
served directly (and fast). Due to this, logging for these requests are
also turned off

#### Usage

    Fire$serve_static(
      at,
      path,
      use_index = TRUE,
      fallthrough = FALSE,
      html_charset = "utf-8",
      headers = list(),
      validation = NULL
    )

#### Arguments

- `at`:

  The url path to listen to requests on

- `path`:

  The path to the file or directory on the file system

- `use_index`:

  Should an `index.html` file be served if present when a client
  requests the folder

- `fallthrough`:

  Should requests that doesn't match a file enter the request loop or
  have a 404 response send directly

- `html_charset`:

  The charset to report for serving html files

- `headers`:

  A list of headers to add to the response. Will be combined with the
  global headers of the app

- `validation`:

  An optional validation pattern. Presently, the only type of validation
  supported is an exact string match of a header. For example, if
  validation is `"abc" = "xyz"`, then HTTP requests must have a header
  named `abc` (case-insensitive) with the value `"xyz"`
  (case-sensitive). If a request does not have a matching header, than
  httpuv will give a 403 Forbidden response. If `character(0)` (the
  default), then no validation check will be performed.

------------------------------------------------------------------------

### Method `exclude_static()`

Exclude a url path from serving static content. Only meaningful to
exclude sub paths of path that are set to serve static content

#### Usage

    Fire$exclude_static(at)

#### Arguments

- `at`:

  The url path to exclude from static serving. Request to this path will
  enter the normal request loop

------------------------------------------------------------------------

### Method [`attach()`](https://rdrr.io/r/base/attach.html)

Attach a plugin to the app. See the [*Creating and using fiery plugins*
vignette](https://fiery.data-imaginist.com/articles/plugins.html) for
more information

#### Usage

    Fire$attach(plugin, ..., name = NULL, force = FALSE)

#### Arguments

- `plugin`:

  The plugin to attach

- `...`:

  Arguments to pass into the plugins `on_attach()` method

- `name`:

  Optional name for the plugin. If omitted `plugin$name` will be used
  instead

- `force`:

  If the plugin has already been attached an error is thrown, unless
  `force = TRUE` which tells the app to reattach it

------------------------------------------------------------------------

### Method `has_plugin()`

Check if the app has a plugin attached

#### Usage

    Fire$has_plugin(name)

#### Arguments

- `name`:

  The name of the plugin

#### Returns

A boolean indicating if the given plugin is already attached

------------------------------------------------------------------------

### Method `header()`

Add a global http header that will be applied to all responses

#### Usage

    Fire$header(name, value)

#### Arguments

- `name`:

  The name of the header

- `value`:

  The value of the header. Use `NULL` to remove the global header

------------------------------------------------------------------------

### Method `set_data()`

Add data to the global data store

#### Usage

    Fire$set_data(name, value)

#### Arguments

- `name`:

  The name identifying the data

- `value`:

  The data to add

------------------------------------------------------------------------

### Method `get_data()`

Retrieve data from the global data store

#### Usage

    Fire$get_data(name)

#### Arguments

- `name`:

  The name identifying the data

#### Returns

The data requested. Returns `NULL` if the store does not contain the
requested data

------------------------------------------------------------------------

### Method `remove_data()`

Remove data from the global data store

#### Usage

    Fire$remove_data(name)

#### Arguments

- `name`:

  The name identifying the data to be removed

------------------------------------------------------------------------

### Method [`time()`](https://rdrr.io/r/stats/time.html)

Add a timed evaluation that will be evaluated after the given number of
seconds.

#### Usage

    Fire$time(expr, then, after, loop = FALSE)

#### Arguments

- `expr`:

  The expression to evaluate when the time has passed

- `then`:

  A handler to call once `expr` has been evaluated

- `after`:

  The time in second to wait before evaluating `expr`

- `loop`:

  Should `expr` be called repeatedly with the interval given by `after`

#### Returns

A unique id identifying the handler

------------------------------------------------------------------------

### Method `remove_time()`

Remove a timed evaluation

#### Usage

    Fire$remove_time(id)

#### Arguments

- `id`:

  The unique id identifying the handler

------------------------------------------------------------------------

### Method `delay()`

Add a delayed evaluation to be evaluated immediately at the end of the
loop cycle.

#### Usage

    Fire$delay(expr, then)

#### Arguments

- `expr`:

  The expression to evaluate at the end of the cycle

- `then`:

  A handler to call once `expr` has been evaluated

#### Returns

A unique id identifying the handler

------------------------------------------------------------------------

### Method `remove_delay()`

Remove a delayed evaluation

#### Usage

    Fire$remove_delay(id)

#### Arguments

- `id`:

  The unique id identifying the handler

------------------------------------------------------------------------

### Method `async()`

**\[deprecated\]** Add an asynchronous evaluation to be evaluated in
another process without blocking the server. This function has been
deprecated in favor of using your own async framework of choice, e.g.
[mirai](https://mirai.r-lib.org) or
[promises](https://rstudio.github.io/promises/)

#### Usage

    Fire$async(expr, then)

#### Arguments

- `expr`:

  The expression to evaluate at the end of the cycle

- `then`:

  A handler to call once `expr` has been evaluated

#### Returns

A unique id identifying the handler

------------------------------------------------------------------------

### Method `remove_async()`

Remove an async evaluation

#### Usage

    Fire$remove_async(id)

#### Arguments

- `id`:

  The unique id identifying the handler

------------------------------------------------------------------------

### Method `set_client_id_converter()`

Sets the function that converts an HTTP request into a specific client
id

#### Usage

    Fire$set_client_id_converter(converter)

#### Arguments

- `converter`:

  A function with the argument `request`

------------------------------------------------------------------------

### Method `set_logger()`

Sets the logging function to use

#### Usage

    Fire$set_logger(logger)

#### Arguments

- `logger`:

  A function with the arguments `event`, `message`, `request`, and `...`

------------------------------------------------------------------------

### Method [`log()`](https://rdrr.io/r/base/Log.html)

Log a message with the logger attached to the app. See
[loggers](https://fiery.data-imaginist.com/reference/loggers.md) for
build in functionality

#### Usage

    Fire$log(
      event,
      message,
      request = NULL,
      ...,
      .logcall = sys.call(),
      .topcall = sys.call(-1),
      .topenv = parent.frame()
    )

#### Arguments

- `event`:

  The event associated with the message

- `message`:

  The message to log

- `request`:

  The `Request` object associated with the message, if any.

- `...`:

  Additional arguments passed on to the logger.

- `.logcall`:

  The call that send the log request

- `.topcall`:

  The call in which `.logcall` is called from

- `.topenv`:

  The environment associated with `.topcall`

------------------------------------------------------------------------

### Method `is_running()`

Test if an app is running

#### Usage

    Fire$is_running()

------------------------------------------------------------------------

### Method `safe_call()`

Evaluate an expression safely, logging any errors, warnings, or messages
that bubbles up

#### Usage

    Fire$safe_call(expr, request = NULL)

#### Arguments

- `expr`:

  An expression to evaluate

- `request`:

  The request under evaluation, if any. Used in logging

#### Returns

The value of the expression. If an error is caught, the condition object
is returned instead

------------------------------------------------------------------------

### Method `test_request()`

Send a request directly to the request logic of a non-running app. Only
intended for testing the request logic

#### Usage

    Fire$test_request(request)

#### Arguments

- `request`:

  The request to send

------------------------------------------------------------------------

### Method `test_header()`

Send a request directly to the header logic of a non-running app. Only
intended for testing the request logic

#### Usage

    Fire$test_header(request)

#### Arguments

- `request`:

  The request to send

------------------------------------------------------------------------

### Method `test_message()`

Send a message directly **to** the message logic of a non-running app.
Only intended for testing the websocket logic

#### Usage

    Fire$test_message(request, binary, message, withClose = TRUE)

#### Arguments

- `request`:

  The request to use to establish the connection

- `binary`:

  Is the message send in binary or character format

- `message`:

  The message to send. If `binary = FALSE` a character vector, if
  `binary = TRUE` a raw vector

- `withClose`:

  Should the websocket connection be closed at the end by the client

------------------------------------------------------------------------

### Method `test_websocket()`

Send a message directly **from** a non-running app. Only intended for
testing the websocket logic

#### Usage

    Fire$test_websocket(request, message, close = TRUE)

#### Arguments

- `request`:

  The request to use to establish the connection

- `message`:

  The message to send from the app

- `close`:

  Should the websocket connection be closed at the end by the server

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Fire$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Create a New App
app <- Fire$new(port = 4689)

# Setup the data every time it starts
app$on('start', function(server, ...) {
    server$set_data('visits', 0)
    server$set_data('cycles', 0)
})

# Count the number of cycles
app$on('cycle-start', function(server, ...) {
    server$set_data('cycles', server$get_data('cycles') + 1)
})

# Count the number of requests
app$on('before-request', function(server, ...) {
    server$set_data('visits', server$get_data('visits') + 1)
})

# Handle requests
app$on('request', function(server, ...) {
    list(
        status = 200L,
        headers = list('Content-Type' = 'text/html'),
        body = paste('This is indeed a test. You are number', server$get_data('visits'))
    )
})

# Show number of requests in the console
app$on('after-request', function(server, ...) {
    message(server$get_data('visits'))
    flush.console()
})

# Terminate the server after 300 cycles
app$on('cycle-end', function(server, ...) {
    if (server$get_data('cycles') > 300) {
        message('Ending...')
        flush.console()
        server$extinguish()
    }
})

# Be polite
app$on('end', function(server) {
    message('Goodbye')
    flush.console()
})

if (FALSE) { # \dontrun{
app$ignite(showcase = TRUE)
} # }
```
