# fiery 0.2.2

* Changed default host to 127.0.0.1
* Fixed test errors on windows builders

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
