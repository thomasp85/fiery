#' Delaying code execution in Fiery
#' 
#' R, and thus `fiery`, is single threaded, meaning that every request must be
#' handled one at a time. Because of this it is of utmost importance to keep the
#' computation time for each request handling as low as possible so that the
#' server does not become unresponsive. Still, sometimes you may need to perform
#' long running computations as part of the server functionality. `fiery` comes
#' with three different facilities for this, each with its own use case. All of
#' them are build on top of [future][future::future].
#' 
#' @section General format: 
#' All three methods have the same general API. They can recieve an expression
#' to evaluate, as well as a `then` function to call once the evaluation
#' eventually completes. The `then` function will recieve the result of the
#' provided expression as well as the server itself. In general, any code that
#' works on the server should be handled by the `then` function as the
#' expression will not necessarily have access to the current environment. Thus,
#' the expression should be as minimal as possible while still containing the
#' heavy part of the calculations, while the `then` function should be used to
#' act upon the result of the expression.
#' 
#' The general format is thus (using `delay()` as an example):
#' 
#' ```
#' app$delay({
#'   # Heavy calculation
#' }, then = function(res, server) {
#'   # Do something with 'res' (the result of the expression) and 'server' the 
#'   # server object itself
#' })
#' ```
#' 
#' @section Pushing execution to the end of a cycle:
#' If it is important to achieve a fast response time, but server congestion is
#' of lesser concern (the server might be used for a local app with only one
#' user at a time), the `delay()` method can be used to push the evaluation of
#' long running computation to the end of the current cycle. It will of course
#' not be possible to return the result of the computation as part of the 
#' response, but e.g. a `202` response can be returned instead indicating that
#' the request is being processed. In that way the client can act accordingly 
#' without appearing frozen. An alternative if a lengthy POST request is 
#' recieved is to return `303` with a reference to the URL where the result can
#' be recieved.
#' 
#' @section Executing in another process:
#' If long running computations are needed and congestion is an issue it does
#' not help to simply push back execution to the end of the cycle as this will 
#' block requests while the code is evaluating. Instead it is possible to use 
#' the `async()` method to evaluate the expression in another thread. This 
#' method uses [future::multiprocess()] to evaluate the expression and may thus 
#' fork the current R process if supported (Unix systems) or start another R
#' session (Windows). At the end of each cycle all async evaluations are checked
#' for completion, and if completed the `then` function will be called with the
#' result. If the async evaluation is not completed it will continue to churn.
#' 
#' @section Executing after a time interval:
#' If code is meant to be evaluated after a certain amount of time has passed,
#' use the `time()` method. In addition to `expr` and `then`, `time()` takes two
#' additional arguments: `after` (the time in seconds to wait before evaluation)
#' and `loop` (whether to repeat the timed evaluation after completion). Using
#' `loop = TRUE` it is e.g. possible to continually check for state changes on
#' the server and e.g. run some specific code if new files appear in a 
#' directory. In the end of each cycle all timed expressions will be checked for
#' whether they should be evaluated and run if their specific time interval has
#' passed.
#' 
#' @section Error handling:
#' As both the expression and `then` function might throw errors they are 
#' evaluated in a safe context and any errors that might occur will be send to 
#' the [server log][logging] without affecting other waiting evaluations.
#' 
#' @name delay_doc
#' @rdname delay_doc
#' @aliases delay async time
#' 
NULL
