# App Logging

`fiery` has a build in logging mechanism that lets you capture event
information however you like. Every user-injested warnings and errors
are automatically captured by the logger along with most system errors
as well. `fiery` tries very hard not to break due to faulty app logic.
This means that any event handler error will be converted to an error
log without `fiery` stopping. In the case of request handlers a 500L
response will be send back if any error is encountered.

## Usage

``` r
logger_null()

logger_console(format = "{time} - {event}: {message}")

logger_file(file, format = "{time} - {event}: {message}")

logger_otel(format = "{time} - {event}: {message}")

logger_switch(..., default = logger_null())

logger_logger(default_level = "INFO")

common_log_format

combined_log_format
```

## Format

An object of class `character` of length 1.

An object of class `character` of length 1.

## Arguments

- format:

  A [glue](https://glue.tidyverse.org/reference/glue.html) string
  specifying the format of the log entry

- file:

  A file or connection to write to

- ...:

  A named list of loggers to use for different events. The same
  semantics as [switch](https://rdrr.io/r/base/switch.html) is used so
  it is possible to let events *fall through* e.g.
  `logger_switch(error =, warning = logger_file('errors.log'))`.

- default:

  A catch-all logger for use with events not defined in `...`

- default_level:

  The log level to use for events that are not `request`, `websocket`,
  `message`, `warning`, or `error`

## Setting a logger

By default, `fiery` uses `logger_null()` which forwards warning and
error messages to
[`stderr()`](https://rdrr.io/r/base/showConnections.html) and ignores
any other logging events. To change this behavior, set a different
logger using the `set_logger()` method:

    app$set_logger(logger)

where `logger` is a function taking at least the following arguments:
`event`, `message`, `request`, `time`, and `...`.

`fiery` comes with some additional loggers, which either writes all logs
to a file or to the console. A new instance of the file logger can be
created with `logger_file(file)`:

    app$set_logger(logger_file('fiery_log.log'))

A new instance of the console logger can be create with
`logger_console()`:

    app$set_logger(logger_console())

Both functions takes a `format` a argument that lets you customise how
the log is written. Furthermore the console logger will style the logs
with colour coding depending on the content if the console supports it.

As a last possibility it is possible to use different loggers dependent
on the event by using the switch logger:

    app$set_logger(logger_switch(warning =,
                                 error = logger_file('errors.log),
                                 default = logger_file('info.log')))

## Automatic logs

`fiery` logs a number of different information by itself describing its
operations during run. The following events are send to the log:

- *start*:

  Will be send when the server starts up

- *resume*:

  Will be send when the server is resumed

- *stop*:

  Will be send when the server stops

- *request*:

  Will be send when a request has been handled. The message will contain
  information about how long time it took to handle the request or if it
  was denied.

- *websocket*:

  Will be send every time a WebSocket connection is established or
  closed as well as when a message is received or send

- *message*:

  Will be send every time a message is emitted by an event handler or
  delayed execution handler

- *warning*:

  Will be send everytime a warning is emitted by an event handler or
  delayed execution handler

- *error*:

  Will be send everytime an error is signaled by an event handler or
  delayed execution handler. In addition some internal functions will
  also emit error event when exceptions are encountered

By default only *message*, *warning* and *error* events will be logged
by sending them to the error stream as a
[`message()`](https://rdrr.io/r/base/message.html).

## Access Logs

Of particular interest are logs that detail requests made to the server.
These are the `request` events detailed above. There are different
standards for how requests are logged. `fiery` uses the *Common Log
Format* by default, but this can be modified by setting the
`access_log_format` field to a
[glue](https://glue.tidyverse.org/reference/glue.html) expression that
has access to the following variables:

- `start_time`:

  The time the request was recieved

- `end_time`:

  The time the response was send back

- `request`:

  The `Request` object

- `response`:

  The `Response` object

- `id`:

  The client id

To change the format:

    app$access_log_format <- combined_log_format

## Custom logs

Apart from the standard logs described above it is also possible to send
messages to the log as you please, e.g. inside event handlers. This is
done through the [`log()`](https://rdrr.io/r/base/Log.html) method where
you at the very least specify an event and a message. In general it is
better to send messages through
[`log()`](https://rdrr.io/r/base/Log.html) rather than with
[`warning()`](https://rdrr.io/r/base/warning.html) and
[`stop()`](https://rdrr.io/r/base/stop.html) even though the latters
will eventually be caught, as it gives you more control over the logging
and what should happen in the case of an exception.

An example of using [`log()`](https://rdrr.io/r/base/Log.html) in a
handler could be:

    app$on('header', function(server, id, request) {
      server$log('info', paste0('request from ', id, ' received'), request)
    })

Which would log the timepoint the headers of a request has been
recieved.
