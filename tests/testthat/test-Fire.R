test_that('handlers can be added, triggered and removed', {
    app <- Fire$new(port = random_port())
    
    triggerRes <- app$trigger('test')
    expect_type(triggerRes, 'list')
    expect_length(triggerRes, 0)
    expect_named(triggerRes, character())
    
    id1 <- app$on('test', function(...) 10)
    triggerRes <- app$trigger('test')
    expect_type(triggerRes, 'list')
    expect_length(triggerRes, 1)
    expect_named(triggerRes, id1)
    expect_equal(triggerRes[[1]], 10)
    
    app$off(id1)
    triggerRes <- app$trigger('test')
    expect_type(triggerRes, 'list')
    expect_length(triggerRes, 0)
    expect_named(triggerRes, character())
})

test_that('Fire objects are printed', {
    app <- Fire$new(port = random_port())
    app$attach(list(
        name = 'test',
        on_attach = function(...) {}
    ))
    app$on('start', function(...){})
    app$on('request', function(...){})
    app$on('request', function(...){})
    
    expect_output(print(app), 'A fiery webserver')
    expect_output(print(app), 'Plugins attached: test')
    expect_output(print(app), 'start: 1')
    expect_output(print(app), 'request: 2')
    
    app <- Fire$new(port = random_port())
    expect_output(print(app), 'Plugins attached: none')
    expect_output(print(app), 'Event handlers added: none')
})

test_that('protected events cannot be triggered', {
    app <- Fire$new(port = random_port())
    
    protected <- c('start', 'resume', 'end', 'cycle-start', 
                   'cycle-end', 'header', 'before-request', 'request', 
                   'after-request', 'before-message', 'message', 
                   'after-message', 'websocket-closed', 'send')
    
    for (i in protected) {
        expect_error(app$trigger(i))
    }
})

test_that('data can be set, get and removed', {
    app <- Fire$new(port = random_port())
    expect_null(app$get_data('test'))
    testdata <- list(a = 1, b = 1:10, c = letters[6:10])
    app$set_data('test', testdata)
    expect_equal(app$get_data('test'), testdata)
    expect_error(app$get_data(1))
    expect_error(app$get_data(c('test', 'test2')))
    app$remove_data('test')
    expect_null(app$get_data('test'))
})

test_that('plugins are being attached', {
    app <- Fire$new(port = random_port())
    app$set_data('test', 10)
    plugin <- list(
        on_attach = function(server, extraPar) {
            server$on('test', function(...){10 + extraPar})
        }
    )
    expect_error(app$attach(plugin, 15))
    plugin$name <- 'plugin'
    app$attach(plugin, 15)
    expect_error(app$attach(plugin, 10))
    expect_equal(app$trigger('test')[[1]], 25)
    plugin2 <- list(
        on_attach = function(...) {message('test')},
        name = 'plugin2',
        require = c('plugin', 'test')
    )
    expect_error(app$attach(plugin2))
    plugin2$require <- 'plugin'
    expect_message(app$attach(plugin2), 'test')
    expect_equal(plugin, app$plugins$plugin)
    expect_error(app$plugins$test <- plugin)
    plugin3 <- list(
        on_attach = function(...) {stop('test')},
        name = 'plugin3'
    )
    expect_snapshot_error(app$attach(plugin3))
})

test_that('id converter can be set and gets called', {
    app <- Fire$new(port = random_port())
    app$on('request', function(server, id, ...) {
        server$set_data('id', id)
    })
    request <- fake_request('http://www.example.com', REMOTE_ADDR = '127.0.0.1')
    app$test_request(request)
    expect_equal(app$get_data('id'), client_to_id(reqres::Request$new(request)))
    
    app$set_client_id_converter(function(request) {
        10
    })
    app$test_request(request)
    expect_equal(app$get_data('id'), 10)
    
    expect_error(app$set_client_id_converter('test'))
    expect_error(app$set_client_id_converter(function(test) {10}))
})

test_that('active bindings work', {
    app <- Fire$new(port = random_port())
    expect_error(app$host <- 10)
    expect_error(app$host <- letters[1:3])
    app$host <- 'test'
    expect_equal(app$host, 'test')
    
    expect_error(app$port <- 'test')
    expect_error(app$port <- 1.5)
    app$port <- 10
    expect_equal(app$port, 10)
    
    expect_error(app$refresh_rate <- 'test')
    expect_error(app$refresh_rate <- 1:5)
    app$refresh_rate <- 10.5
    expect_equal(app$refresh_rate, 10.5)
    
    expect_error(app$refresh_rate_nb <- 'test')
    expect_error(app$refresh_rate_nb <- 1:5)
    app$refresh_rate_nb <- 10.5
    expect_equal(app$refresh_rate_nb, 10.5)
    
    expect_error(app$trigger_dir <- 'test')
    expect_error(app$trigger_dir <- 1:5)
    dir <- tempdir()
    app$trigger_dir <- dir
    expect_equal(app$trigger_dir, dir)
})

test_that('life cycle events get fired', {
    app <- Fire$new(port = random_port())
    app$on('start', function(server, ...) {
        server$set_data('events', c(server$get_data('events'), 'start'))
    })
    app$on('resume', function(server, ...) {
        server$set_data('events', c(server$get_data('events'), 'resume'))
    })
    app$on('cycle-start', function(server, ...) {
        server$set_data('events', c(server$get_data('events'), 'cycle-start'))
    })
    app$on('cycle-end', function(server, ...) {
        server$set_data('events', c(server$get_data('events'), 'cycle-end'))
        if (sum(server$get_data('events') == 'cycle-end') == 2) {
            server$extinguish()
        }
    })
    app$on('end', function(server, ...) {
        server$set_data('events', c(server$get_data('events'), 'end'))
    })
    app$ignite()
    igniteRes <- app$get_data('events')
    app$remove_data('events')
    app$start()
    startRes <- app$get_data('events')
    app$remove_data('events')
    app$reignite()
    reigniteRes <- app$get_data('events')
    app$remove_data('events')
    app$resume()
    resumeRes <- app$get_data('events')
    app$remove_data('events')
    
    expect_equal(igniteRes, startRes)
    expect_equal(igniteRes, c('start', 'cycle-start', 'cycle-end', 'cycle-start', 'cycle-end', 'end'))
    expect_equal(reigniteRes, resumeRes)
    expect_equal(reigniteRes, c('start', 'resume', 'cycle-start', 'cycle-end', 'cycle-start', 'cycle-end', 'end'))
    
    app$refresh_rate_nb <- 0.001
    app$ignite(block = FALSE)
    # This no longer works in later. Messages generated during run_now() can't 
    # get captured
#    expect_silent({ # Need to force some cycles to happen
#        Sys.sleep(.1)
#        later::run_now()
#        Sys.sleep(.1)
#        later::run_now()
#    }) #, 'Cannot stop server from within a non-blocking event cycle')
    app$stop()
    igniteResNoBlock <- app$get_data('events')
    app$remove_data('events')
    expect_equal(unique(igniteResNoBlock), c('start', 'cycle-start', 'cycle-end', 'end'))
    app$reignite(block = FALSE)
    app$extinguish()
    reigniteResNoBlock <- app$get_data('events')
    app$remove_data('events')
    expect_equal(reigniteResNoBlock, c('start', 'resume', 'cycle-start', 'cycle-end', 'end'))
})

test_that('request events fire', {
    app <- Fire$new(port = random_port())
    request <- fake_request('http://www.example.com')
    
    app$on('before-request', function(server, ...) {
        server$set_data('events', c(server$get_data('events'), 'before'))
        list(test = 4)
    })
    app$on('request', function(server, arg_list, id, request, ...) {
        server$set_data('events', c(server$get_data('events'), 'during'))
        server$set_data('passed_args', arg_list$test)
        request$respond()$status_with_text(200L)
    })
    app$on('after-request', function(server, request, ...) {
        server$set_data('events', c(server$get_data('events'), 'after'))
        server$set_data('passed_response', request$respond()$as_list())
    })
    response <- app$test_request(request)
    
    expect_equal(app$get_data('events'), c('before', 'during', 'after'))
    expect_equal(app$get_data('passed_args'), 4)
    expect_equal(app$get_data('passed_response'), response)
})

test_that('message events fire', {
    app <- Fire$new(port = random_port())
    request <- fake_request('http://www.example.com')
    
    app$on('before-message', function(server, ...) {
        server$set_data('events', c(server$get_data('events'), 'before'))
        list(test = 4, message = 'test2', binary = FALSE)
    })
    app$on('message', function(server, message, arg_list, ...) {
        server$set_data('events', c(server$get_data('events'), 'during'))
        server$set_data('passed_args', list(test = arg_list$test, message = message))
    })
    app$on('after-message', function(server, response, ...) {
        server$set_data('events', c(server$get_data('events'), 'after'))
    })
    app$test_message(request, FALSE, 'test')
    
    expect_equal(app$get_data('events'), c('before', 'during', 'after'))
    expect_equal(app$get_data('passed_args'), list(test = 4, message = 'test2'))
    
    app <- Fire$new(port = random_port())
    request <- fake_request('http://www.example.com')
    
    app$on('message', function(server, message, arg_list, ...) {
        server$set_data('events', c(server$get_data('events'), 'during'))
        server$set_data('passed_args', list(test = arg_list$test, message = message))
    })
    app$on('after-message', function(server, response, ...) {
        server$set_data('events', c(server$get_data('events'), 'after'))
    })
    app$test_message(request, FALSE, 'test')
    
    expect_equal(app$get_data('events'), c('during', 'after'))
})

test_that('header event fire', {
    app <- Fire$new(port = random_port())
    request <- fake_request('http://www.example.com')
    expect_true(is.null(app$test_header(request)))
    app$on('header', function(server, ...) {
        server$set_data('header', TRUE)
        TRUE
    })
    app$test_header(request)
    expect_true(app$get_data('header'))
    app$on('header', function(server, request, ...) {
        request$respond()$status_with_text(500L)
        FALSE
    })
    response <- app$test_header(request)
    expect_equal(response$status, 500L)
})

test_that('errors in start and resume gets caught', {
    app <- Fire$new(port = random_port())
    app$set_logger(logger_console())
    app$on('start', function(...) {
        stop('Testing an error')
    })
    expect_output({
        app$ignite(silent = TRUE, block = FALSE)
        later::run_now()
    }, 'Testing an error')
    capture_output(app$extinguish())
    app <- Fire$new(port = random_port())
    app$set_logger(logger_console())
    app$on('resume', function(...) {
        stop('Testing an error')
    })
    expect_output({
        app$reignite(silent = TRUE, block = FALSE)
        later::run_now()
    }, 'Testing an error')
    capture_output(app$extinguish())
})

test_that('futures can be added and called', {
    app <- Fire$new(port = random_port())

    app$delay({
        10
    }, function(res, server, ...) {
        message(res)
        server$extinguish()
    })
    app$on('cycle-end', function(server, ...) {
        server$extinguish()
    })
    expect_message(app$ignite(), '10')
    
    id <- app$delay({
        10
    }, function(res, server, ...) {
        message(res)
        server$extinguish()
    })
    app$on('cycle-end', function(server, ...) {
        server$extinguish()
    })
    app$remove_delay(id)
    expect_silent(app$ignite(silent = TRUE))

    app <- Fire$new(port = random_port())
    app$time({
        10
    }, function(res, server, ...) {
        message(res)
        server$extinguish()
    }, 1)
    app$on('start', function(server, ...) server$set_data('time', Sys.time()))
    app$on('cycle-end', function(server, ...) {
        start <- server$get_data('time')
        if (Sys.time() - start > 2) {
            server$extinguish()
        }
    })
    expect_message(app$ignite(), '10')

    app <- Fire$new(port = random_port())
    id <- app$time({
        10
    }, function(res, server, ...) {
        message(res)
        server$extinguish()
    }, 1)
    app$remove_time(id)
    app$on('start', function(server, ...) server$set_data('time', Sys.time()))
    app$on('cycle-end', function(server, ...) {
        start <- server$get_data('time')
        if (Sys.time() - start > 2) {
            server$extinguish()
        }
    })
    expect_silent(app$ignite(silent = TRUE))

    skip_on_os('windows') # The async stuff fail on windows builders though it works fine locally
    app <- Fire$new(port = random_port())
    id <- app$async({
        10
    }, function(res, server, ...) {
        message(res)
        server$extinguish()
    })
    app$remove_async(id)
    app$on('start', function(server, ...) server$set_data('time', Sys.time()))
    app$on('cycle-end', function(server, ...) {
        start <- server$get_data('time')
        if (Sys.time() - start > 2) {
            server$extinguish()
        }
    })
    expect_silent(app$ignite(silent = TRUE))
    
    app <- Fire$new(port = random_port())
    app$async({
        10
    }, function(res, server, ...) {
        message(res)
        server$extinguish()
    })
    app$on('start', function(server, ...) server$set_data('time', Sys.time()))
    app$on('cycle-end', function(server, ...) {
        start <- server$get_data('time')
        if (Sys.time() - start > 2) {
            server$extinguish()
        }
    })
    expect_message(app$ignite(), '10')
})

test_that('ignite is blocked during run', {
    skip_on_cran()
    app <- Fire$new(port = random_port())
    app$set_logger(logger_console())
    app$refresh_rate_nb <- 0.001

    capture_output(app$ignite(block = FALSE))
    expect_output({
        app$ignite()
        later::run_now()
    }, 'Server is already running and cannot be started')
    capture_output(app$extinguish())
})

test_that('external triggers are fired', {
    app <- Fire$new(port = random_port())
    app$set_logger(logger_console())

    dir <- tempdir()
    app$trigger_dir <- dir

    app$on('test', function(server, ...) {
        server$set_data('ext_args', list(...))
    })
    saveRDS(4, file.path(dir, 'testfail.rds'))
    saveRDS(list(test = 'test'), file.path(dir, 'test.rds'))
    expect_output({
        app$ignite(silent = TRUE, block = FALSE)
        later::run_now()
        app$extinguish()
    }, 'External triggers must be an rds file containing a list')
    expect_equal(list(test = 'test'), app$get_data('ext_args'))
})

test_that('websockets are attached, and removed', {
    app <- Fire$new(port = random_port())
    req <- fake_request('http://www.example.com')
    app$on('send', function(server, ...) {server$set_data('send', TRUE)})
    expect_null(app$get_data('send'))
    expect_message(app$test_websocket(req, 'test', FALSE), 'test')
    expect_true(app$get_data('send'))
    expect_message(app$send('keep testing', client_to_id(req)), 'keep testing')
    expect_message(app$send('keep testing again'), 'keep testing again')
    expect_message(app$close_ws_con(client_to_id(req)), 'closing')
    expect_silent(app$close_ws_con(client_to_id(req)))
    expect_silent(app$send('keep testing', client_to_id(req)))
    expect_silent(app$send('keep testing again'))
})

test_that('showcase opens a browser', {
    oldopt <- options(browser = function(url) message('Open browser'))
    
    app <- Fire$new(port = random_port())
    app$on('cycle-end', function(server, ...) server$extinguish())
    
    expect_message(app$ignite(showcase = TRUE), 'Open browser')
    expect_message(app$ignite(showcase = TRUE, block = FALSE), 'Open browser')
    app$extinguish()
    options(oldopt)
})

test_that('global headers are assigned and used', {
    app <- Fire$new(port = random_port())
    app$header('X-Powered-By', 'fiery')
    app$header('X-XSS-Protection', '1; mode=block')
    app$on('request', function(request, ...) {
        request$respond()$status_with_text(200L)
    })
    response <- app$test_request(fake_request('www.example.com'))
    expect_equal(response$headers, list('Content-Type' = 'text/plain', 'X-Powered-By' = 'fiery', 'X-XSS-Protection' = '1; mode=block'))
    app$header('X-XSS-Protection', NULL)
    response <- app$test_request(fake_request('www.example.com'))
    expect_equal(response$headers, list('Content-Type' = 'text/plain', 'X-Powered-By' = 'fiery'))
    expect_equal(app$header('X-Powered-By'), 'fiery')
})

test_that('app can be mounted at path', {
    app <- Fire$new(port = random_port())
    app$set_logger(logger_console('{message}'))
    
    expect_equal(app$root, '')
    expect_error(app$root <- 123)
    expect_error(app$root <- c('test', 'test2'))
    app$root <- '//test/'
    expect_equal(app$root, '/test')
    req <- fake_request('http://example.com/test/testing')
    app$test_request(req)
    expect_equal(req$PATH_INFO, '/testing')
    req <- fake_request('http://example.com/test/testing')
    app$test_header(req)
    expect_equal(req$PATH_INFO, '/testing')
    req <- fake_request('http://example.com/test/testing')
    app$set_logger(logger_null())
    expect_message(app$test_websocket(req, 'test'), 'test')
    expect_equal(req$PATH_INFO, '/testing')
    
    app$set_logger(logger_console('{message}'))
    req <- fake_request('http://example.com/testing')
    expect_snapshot_output(res <- app$test_request(req))
    expect_equal(res$status, 400L)
    req <- fake_request('http://example.com/testing')
    expect_snapshot_output(res <- app$test_header(req))
    expect_equal(res$status, 400L)
    req <- fake_request('http://example.com/testing')
    expect_snapshot_output(app$test_websocket(req, 'test'))
})

test_that("Logging can be configured", {
    app <- Fire$new(port = random_port())
    expect_equal(app$access_log_format, common_log_format)
    app$access_log_format <- combined_log_format
    expect_equal(app$access_log_format, combined_log_format)
    app$on('test', function(server, ...) {
        server$log('test', 'this is a test')
    })
    app$set_logger(logger_console())
    expect_output(app$trigger('test'), 'test: this is a test')
    
    app$access_log_format <- common_log_format
    expect_output(app$test_request(fake_request('www.example.com/path', REMOTE_ADDR = 'test')), 'request: test - ID_test \\[\\d{2}/\\w+/\\d{4}:\\d{2}:\\d{2}:\\d{2} +|-\\d{4}\\] "GET /path HTTP/1\\.1" 404 0')
})

test_that('is_running works', {
    app <- Fire$new(port = random_port())
    expect_false(app$is_running())
    app$on('cycle-start', function(server, ...) {
        server$log('message', server$is_running())
    })
    app$on('cycle-end', function(server, ...) {
        server$extinguish()
    })
    expect_message(app$ignite(), 'message: TRUE')
    expect_false(app$is_running())
    
    app <- Fire$new(port = random_port())
    app$ignite(block = FALSE)
    expect_true(app$is_running())
    app$extinguish()
    expect_false(app$is_running())
})

test_that('safe_call catches conditions', {
  app <- Fire$new(port = random_port())
  private <- environment(app$clone)$private
  expect_message(private$safe_call(stop('error test')), 'error: error test')
  expect_message(private$safe_call(warning('warning test')), 'warning: warning test')
  expect_message(private$safe_call(message('message test')), 'message: message test')
})
