# fiery 0.2.3.9000

* `Fire$new()` now takes a port and host argument to set these fields on 
  initialisation. (fixes #5)
* Results from before-request and before-message events are now passed on to the
  request and message handlers as a list in the `arg_list` argument rather than
  as single arguments.
* Fire objects now has a print method (#12)

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
