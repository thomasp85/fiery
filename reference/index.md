# Package index

## The Fire object

fiery is build on top of the R6 class system and all functionality in
the package is more or less part of the Fire class. The understanding of
this class is thus integral to understanding fiery.

- [`Fire`](https://fiery.data-imaginist.com/reference/Fire.md) :
  Generate a New App Object

## Logging

Logs are an important part of many servers and fiery has a central
logging facility where callbacks and handlers can send logging
information to without worrying about where and how the logs get
written.

- [`logger_null()`](https://fiery.data-imaginist.com/reference/loggers.md)
  [`logger_conditions()`](https://fiery.data-imaginist.com/reference/loggers.md)
  [`logger_console()`](https://fiery.data-imaginist.com/reference/loggers.md)
  [`logger_file()`](https://fiery.data-imaginist.com/reference/loggers.md)
  [`logger_otel()`](https://fiery.data-imaginist.com/reference/loggers.md)
  [`logger_switch()`](https://fiery.data-imaginist.com/reference/loggers.md)
  [`logger_logger()`](https://fiery.data-imaginist.com/reference/loggers.md)
  [`common_log_format`](https://fiery.data-imaginist.com/reference/loggers.md)
  [`combined_log_format`](https://fiery.data-imaginist.com/reference/loggers.md)
  : App Logging

## Request and response objects

A lot of the logic in web servers are concerned with parsing requests
and building up responses. fiery uses the Request and Response classes
defined in the [reqres](https://github.com/thomasp85/reqres) package.
See the documentation there.

## Routing

A central part of many servers is the router that takes care of
recieving requests and directing them at the right handlers for further
processing. Being the minimal server framework that it is, fiery doesn’t
include routing functionality itself. An option is the
[routr](https://github.com/thomasp85/routr) package which is build to
work together with fiery as a plugin.

## Helpers

Most functionality is available to the user through the Fire object or
loggers. Any additional helper functions provided by fiery is documented
here.

- [`session_id_cookie()`](https://fiery.data-imaginist.com/reference/session_id_cookie.md)
  : Use a session cookie to store the id of the session
