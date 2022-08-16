# fiery 1.1.4

* General upkeep (new CI, redocument, etc)

# fiery 1.1.3

* Avoid tests that fails on some CRAN machines due to new testthat version

# fiery 1.1.2

* Fix bug where delayed logging would result in wrong message being logged (#39)
* Remove some tests as new later architecture makes certain async operations 
  untestable (#40)
* pkgdown site now available at <https://fiery.data-imaginist.com>

# fiery 1.1.1

* Fix bug with root mounting of app where the root would be stripped before
  checking if it exists.
* Fix a bug when evaluating multiple futures at once, where the removal of the
  futures would throw an error (#28)
* Fix a bug preventing setting loggers on cloned apps (#30)
* The call that raises a caught error is now recorded in the log (#33)

# fiery 1.1.0

* Add logging API. Set custom loggers with `set_logger()` and send messages to
  the log with `log()`. Logging is automatically delayed so it doesn't slow down
  request and message handling (#18).
* Added `access_log_format` field to define how requests are logged.
* Added `is_running()` method to query the state of the server.
* Capture errors in each handler for events and delayed execution, so that 
  evaluation of the other handlers are unaffected (#20).
* Document the use of delayed evaluation. See `?delay_doc`

# fiery 1.0.0

* `Fire$new()` now takes a port and host argument to set these fields on 
  initialisation. (fixes #5)
* **BREAKING** Results from before-request and before-message events are now 
  passed on to the request and message handlers as a list in the `arg_list` 
  argument rather than as single arguments.
* The host and port are now advertised when a server is started/resumed (#11)
* Fire objects now has a print method (#12)
* **BREAKING** fiery now uses the `reqres` Request and Response classes for 
  handling http exchange.
* **BREAKING** `attach()` now expect a `on_attach()` method rather than a 
  `onAttach()` method from the plugin. It also expects a `name` field and 
  optionally a `require` field
* **BREAKING** The `header` event now expect handlers to return a logical, with
  `TRUE` indicating further processing, and `FALSE` indicating termination.
* Cycle events are now triggered when running with `block = FALSE` making the
  two run modes identical in their life cycle events.
* **BREAKING** The `after-request` event will no longer pass the response to
  handlers. This can be retrieved from the `request` object.
* The server can now be mounted at a path, which will strip that path from 
  request paths thus making the app logic independent on mounting. Use the 
  `root` field to access and change the root location.
* Websocket connections can now be closed from the server by using the 
  `close_ws_con()` method.
* Better documentation. Events and plugins now has their own documentation 
  entries (fixes #10).
* Convert roxygen documentation to md format
* **BREAKING** fields now uses snake_case rather than camelCase for a more
  consistent interface. This means `refreshRate` -> `refresh_rate`, 
  `triggerDir` -> `trigger_dir`.
* Switch to MIT License
* Catch errors in start and resume event handlers

# fiery 0.2.3

* DelayStack uses `sequential` futures with `lazy = TRUE` because previously 
  used `lazy` futures are deprecated

# fiery 0.2.2

* Changed default host to 127.0.0.1
* Fixed test errors on Windows builders

# fiery 0.2.0

* Added `fake_request` to generate fake, rook-compliant, request objects. Useful
for testing
* Added `header` method to `Fire` for setting global header policies
* Added standard 4xx responses
* Added `FutureStack` class and subclasses to capture expressions for later, 
timed, and async evaluation
* Added `delay`, `remove_delay`, `time`, `remove_time`, `async`, and 
`remove_async` methods to `Fire` for adding delayed, timed, and async 
expressions for evaluation

# fiery 0.1.0

* Added Fire class encapsulating the server runtime
* Added HandlerStack class to store and trigger event handlers
