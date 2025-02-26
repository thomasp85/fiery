standard_app <- function(silent = TRUE) {
    app <- Fire$new(port = 49925)
    if (!silent) {
        app$set_logger(logger_console("{event}: {message}"))
    }
    app$access_log_format <- '{request$ip} - {id} [29/Jan/2025:08:17:44 +0100] "{toupper(request$method)} {request$path}{request$querystring} {toupper(request$protocol)}/1.1" {response$status} {response$content_length()}'
    app
}

test_that('handlers can be added, triggered and removed', {
    app <- standard_app()

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
    skip_on_os("windows") # Windows not a fan of unicode

    app <- standard_app()
    expect_snapshot(app$format())

    app$attach(list(
        name = 'test',
        on_attach = function(...) {}
    ))
    app$on('start', function(...){})
    app$on('request', function(...){})
    app$on('request', function(...){})

    expect_snapshot(app$format())
})

test_that('protected events cannot be triggered', {
    app <- standard_app()

    protected <- c('start', 'resume', 'end', 'cycle-start',
                   'cycle-end', 'header', 'before-request', 'request',
                   'after-request', 'before-message', 'message',
                   'after-message', 'websocket-closed', 'send')

    for (i in protected) {
        expect_snapshot(app$trigger(i), error = TRUE)
    }
})

test_that('data can be set, get and removed', {
    app <- standard_app()
    expect_null(app$get_data('test'))
    testdata <- list(a = 1, b = 1:10, c = letters[6:10])
    app$set_data('test', testdata)
    expect_equal(app$get_data('test'), testdata)
    expect_snapshot(app$get_data(1), error = TRUE)
    expect_snapshot(app$get_data(c('test', 'test2')), error = TRUE)
    app$remove_data('test')
    expect_null(app$get_data('test'))
})

test_that('plugins are being attached', {
    app <- standard_app()
    app$set_data('test', 10)
    plugin <- list(
        on_attach = function(server, extraPar) {
            server$on('test', function(...) 10 + extraPar)
        }
    )
    expect_snapshot(app$attach(plugin, 15), error = TRUE)
    plugin$name <- 'plugin'
    app$attach(plugin, 15)
    expect_snapshot(app$attach(plugin, 10), error = TRUE)
    expect_equal(app$trigger('test')[[1]], 25)
    plugin2 <- list(
        on_attach = function(...) message('test'),
        name = 'plugin2',
        require = c('plugin', 'test')
    )
    expect_snapshot(app$attach(plugin2), error = TRUE)
    plugin2$require <- 'plugin'
    expect_snapshot(app$attach(plugin2))
    expect_equal(plugin, app$plugins$plugin)
    expect_snapshot(app$plugins$test <- plugin, error = TRUE)
    plugin3 <- list(
        on_attach = function(...) stop('test'),
        name = 'plugin3'
    )
    expect_snapshot(app$attach(plugin3), error = TRUE)
})

test_that('id converter can be set and gets called', {
    app <- standard_app()
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

    expect_snapshot(app$set_client_id_converter('test'), error = TRUE)
    expect_snapshot(app$set_client_id_converter(function(test) {10}), error = TRUE)
})

test_that('active bindings work', {
    app <- standard_app()
    expect_snapshot(app$host <- 10, error = TRUE)
    expect_snapshot(app$host <- letters[1:3], error = TRUE)
    app$host <- 'test'
    expect_equal(app$host, 'test')

    expect_snapshot(app$port <- 'test', error = TRUE)
    expect_snapshot(app$port <- 1.5, error = TRUE)
    app$port <- 10
    expect_equal(app$port, 10)

    expect_snapshot(app$refresh_rate <- 'test', error = TRUE)
    expect_snapshot(app$refresh_rate <- 1:5, error = TRUE)
    app$refresh_rate <- 10.5
    expect_equal(app$refresh_rate, 10.5)

    expect_snapshot(app$refresh_rate_nb <- 'test', error = TRUE)
    expect_snapshot(app$refresh_rate_nb <- 1:5, error = TRUE)
    app$refresh_rate_nb <- 10.5
    expect_equal(app$refresh_rate_nb, 10.5)

    expect_snapshot(app$trigger_dir <- 'test', error = TRUE)
    expect_snapshot(app$trigger_dir <- 1:5, error = TRUE)
    dir <- tempdir()
    app$trigger_dir <- dir
    expect_equal(app$trigger_dir, dir)
})

test_that('life cycle events get fired', {
    app <- standard_app()
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
    app$ignite(silent = TRUE)
    igniteRes <- app$get_data('events')
    app$extinguish()
    app$remove_data('events')
    app$start(silent = TRUE)
    startRes <- app$get_data('events')
    app$extinguish()
    app$remove_data('events')
    app$reignite(silent = TRUE)
    reigniteRes <- app$get_data('events')
    app$extinguish()
    app$remove_data('events')
    app$resume(silent = TRUE)
    resumeRes <- app$get_data('events')
    app$extinguish()
    app$remove_data('events')

    expect_equal(igniteRes, startRes)
    expect_equal(igniteRes, c('start', 'cycle-start', 'cycle-end', 'cycle-start', 'cycle-end', 'end'))
    expect_equal(reigniteRes, resumeRes)
    expect_equal(reigniteRes, c('start', 'resume', 'cycle-start', 'cycle-end', 'cycle-start', 'cycle-end', 'end'))

    app$refresh_rate_nb <- 0.001
    app$ignite(block = FALSE, silent = TRUE)
    # This no longer works in later. Messages generated during run_now() can't
    # get captured
#    expect_silent({ # Need to force some cycles to happen
#        Sys.sleep(.1)
#        later::run_now()
#        Sys.sleep(.1)
#        later::run_now()
#    }) #, 'Cannot stop server from within a non-blocking event cycle')
    app$extinguish()
    igniteResNoBlock <- app$get_data('events')
    app$remove_data('events')
    expect_equal(unique(igniteResNoBlock), c('start', 'cycle-start', 'cycle-end', 'end'))
    app$reignite(block = FALSE, silent = TRUE)
    app$extinguish()
    reigniteResNoBlock <- app$get_data('events')
    app$remove_data('events')
    expect_equal(reigniteResNoBlock, c('start', 'resume', 'cycle-start', 'cycle-end', 'end'))
})

test_that('request events fire', {
    app <- standard_app()
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
    app <- standard_app()
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

    app <- standard_app()
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
    app <- standard_app()
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
    app <- standard_app()
    app$on('start', function(...) {
        stop('Testing an error')
    })
    expect_snapshot({
        app$ignite(silent = TRUE, block = FALSE)
        later::run_now()
    })
    capture_output(app$extinguish())
    app <- standard_app()
    app$on('resume', function(...) {
        stop('Testing an error')
    })
    expect_snapshot({
        app$reignite(silent = TRUE, block = FALSE)
        later::run_now()
    })
    capture_output(app$extinguish())
})

test_that('futures can be added and called', {
    app <- standard_app()

    app$delay({
        10
    }, function(res, server, ...) {
        message(res)
        server$extinguish()
    })
    app$on('cycle-end', function(server, ...) {
        server$extinguish()
    })
    expect_snapshot(app$ignite(silent = TRUE))

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

    app <- standard_app()
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
    expect_snapshot(app$ignite(silent = TRUE))

    app <- standard_app()
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
    app <- standard_app()
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

    app <- standard_app()
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
    expect_snapshot(app$ignite())
})

test_that('ignite is blocked during run', {
    skip_on_cran()
    app <- standard_app()
    app$refresh_rate_nb <- 0.001

    app$ignite(block = FALSE, silent = TRUE)
    expect_snapshot({
        app$ignite()
        later::run_now()
    })
    app$extinguish()
})

test_that('external triggers are fired', {
    app <- standard_app()

    dir <- tempdir()
    app$trigger_dir <- dir

    app$on('test', function(server, ...) {
        server$set_data('ext_args', list(...))
    })
    saveRDS(4, file.path(dir, 'testfail.rds'))
    saveRDS(list(test = 'test'), file.path(dir, 'test.rds'))
    expect_snapshot({
        app$ignite(silent = TRUE, block = FALSE)
        later::run_now()
        app$extinguish()
    })
    expect_equal(list(test = 'test'), app$get_data('ext_args'))
})

test_that('websockets are attached, and removed', {
    app <- standard_app()
    req <- fake_request('http://www.example.com')
    app$on('send', function(server, ...) {server$set_data('send', TRUE)})
    expect_null(app$get_data('send'))
    expect_snapshot(app$test_websocket(req, 'test', FALSE))
    expect_true(app$get_data('send'))
    expect_snapshot(app$send('keep testing', client_to_id(req)))
    expect_snapshot(app$send('keep testing again'))
    expect_snapshot(app$close_ws_con(client_to_id(req)))
    expect_silent(app$close_ws_con(client_to_id(req)))
    expect_silent(app$send('keep testing', client_to_id(req)))
    expect_silent(app$send('keep testing again'))
})

test_that('showcase opens a browser', {
    oldopt <- options(browser = function(url) message('Open browser'))

    app <- standard_app()
    app$on('cycle-end', function(server, ...) server$extinguish())

    expect_snapshot(app$ignite(showcase = TRUE))
    expect_snapshot(app$ignite(showcase = TRUE, block = FALSE))
    app$extinguish()
    options(oldopt)
})

test_that('global headers are assigned and used', {
    app <- standard_app()
    app$header('X-Powered-By', 'fiery')
    app$header('X-XSS-Protection', '1; mode=block')
    app$on('request', function(request, ...) {
        request$respond()$status_with_text(200L)
    })
    response <- app$test_request(fake_request('www.example.com'))
    expect_equal(response$headers, list('Content-Type' = 'text/plain', 'X-Powered-By' = 'fiery', 'Date' = response$headers$Date, 'X-XSS-Protection' = '1; mode=block'))
    app$header('X-XSS-Protection', NULL)
    response <- app$test_request(fake_request('www.example.com'))
    expect_equal(response$headers, list('Content-Type' = 'text/plain', 'X-Powered-By' = 'fiery', 'Date' = response$headers$Date))
    expect_equal(app$header('X-Powered-By'), 'fiery')
})

test_that('app can be mounted at path', {
    app <- standard_app()

    expect_equal(app$root, '')
    expect_snapshot(app$root <- 123, error = TRUE)
    expect_snapshot(app$root <- c('test', 'test2'), error = TRUE)
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
    expect_snapshot(app$test_websocket(req, 'test'))
    expect_equal(req$PATH_INFO, '/testing')

    req <- fake_request('http://example.com/testing')
    expect_snapshot(res <- app$test_request(req))
    expect_equal(res$status, 400L)
    req <- fake_request('http://example.com/testing')
    expect_snapshot(res <- app$test_header(req))
    expect_equal(res$status, 400L)
    req <- fake_request('http://example.com/testing')
    expect_snapshot(app$test_websocket(req, 'test'))
})

test_that("Logging can be configured", {
    app <- standard_app(FALSE)
    old_format <- app$access_log_format
    app$access_log_format <- combined_log_format
    expect_equal(app$access_log_format, combined_log_format)
    app$on('test', function(server, ...) {
        server$log('test', 'this is a test')
    })
    expect_snapshot(res <- app$trigger('test'))

    app$access_log_format <- old_format
    expect_snapshot(res <- app$test_request(fake_request('www.example.com/path', REMOTE_ADDR = 'test')))
})

test_that('is_running works', {
    app <- standard_app()
    expect_false(app$is_running())
    app$on('cycle-start', function(server, ...) {
        server$log('message', server$is_running())
    })
    app$on('cycle-end', function(server, ...) {
        server$extinguish()
    })
    expect_snapshot(app$ignite(silent = TRUE))
    expect_false(app$is_running())

    app <- standard_app()
    app$ignite(block = FALSE, silent = TRUE)
    expect_true(app$is_running())
    app$extinguish()
    expect_false(app$is_running())
})

test_that('safe_call catches conditions', {
  app <- standard_app()
  self <- environment(app$clone)$self
  expect_snapshot(cnd <- self$safe_call(stop('error test')))
  expect_snapshot(cnd <- self$safe_call(warning('warning test')))
  expect_snapshot(cnd <- self$safe_call(message('message test')))
})
