context("Fire")

test_that('handlers can be added, triggered and removed', {
    app <- Fire$new()
    
    triggerRes <- app$trigger('test')
    expect_is(triggerRes, 'list')
    expect_length(triggerRes, 0)
    expect_named(triggerRes, character())
    
    id1 <- app$on('test', function(...) 10)
    triggerRes <- app$trigger('test')
    expect_is(triggerRes, 'list')
    expect_length(triggerRes, 1)
    expect_named(triggerRes, id1)
    expect_equal(triggerRes[[1]], 10)
    
    app$off(id1)
    triggerRes <- app$trigger('test')
    expect_is(triggerRes, 'list')
    expect_length(triggerRes, 0)
    expect_named(triggerRes, character())
})

test_that('protected events cannot be triggered', {
    app <- Fire$new()
    
    protected <- c('start', 'resume', 'end', 'cycle-start', 
                   'cycle-end', 'header', 'before-request', 'request', 
                   'after-request', 'before-message', 'message', 
                   'after-message', 'websocket-closed', 'send')
    
    for (i in protected) {
        expect_error(app$trigger(i))
    }
})

test_that('data can be set, get and removed', {
    app <- Fire$new()
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
    app <- Fire$new()
    app$set_data('test', 10)
    plugin <- list(
        onAttach = function(server, extraPar) {
            server$get_data('test') + extraPar
        }
    )
    attachResult <- app$attach(plugin, 15)
    expect_equal(attachResult, 25)
})

test_that('id converter can be set and gets called', {
    app <- Fire$new()
    app$on('request', function(server, id, ...) {
        server$set_data('id', id)
    })
    request <- fake_request('http://www.example.com', REMOTE_ADDR = '127.0.0.1')
    app$test_request(request)
    expect_equal(app$get_data('id'), client_to_id(request))
    
    app$set_client_id_converter(function(request) {
        10
    })
    app$test_request(request)
    expect_equal(app$get_data('id'), 10)
    
    expect_error(app$set_client_id_converter('test'))
    expect_error(app$set_client_id_converter(function(test) {10}))
})

test_that('active bindings work', {
    app <- Fire$new()
    expect_error(app$host <- 10)
    expect_error(app$host <- letters[1:3])
    app$host <- 'test'
    expect_equal(app$host, 'test')
    
    expect_error(app$port <- 'test')
    expect_error(app$port <- 1.5)
    app$port <- 10
    expect_equal(app$port, 10)
    
    expect_error(app$refreshRate <- 'test')
    expect_error(app$refreshRate <- 1:5)
    app$refreshRate <- 10.5
    expect_equal(app$refreshRate, 10.5)
    
    expect_error(app$triggerDir <- 'test')
    expect_error(app$triggerDir <- 1:5)
    dir <- tempdir()
    app$triggerDir <- dir
    expect_equal(app$triggerDir, dir)
})

test_that('lifecycle events get fired', {
    app <- Fire$new()
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
    
    app$ignite(block = FALSE)
    app$stop()
    igniteResNoBlock <- app$get_data('events')
    app$remove_data('events')
    expect_equal(igniteResNoBlock, c('start', 'end'))
    app$reignite(block = FALSE)
    app$extinguish()
    reigniteResNoBlock <- app$get_data('events')
    app$remove_data('events')
    expect_equal(reigniteResNoBlock, c('start', 'resume', 'end'))
})

test_that('request events fire', {
    app <- Fire$new()
    request <- fake_request('http://www.example.com')
    
    app$on('before-request', function(server, ...) {
        server$set_data('events', c(server$get_data('events'), 'before'))
        list(test = 4)
    })
    app$on('request', function(server, ...) {
        server$set_data('events', c(server$get_data('events'), 'during'))
        server$set_data('passed_args', list(...)$test)
        list(status = 200)
    })
    app$on('after-request', function(server, response, ...) {
        server$set_data('events', c(server$get_data('events'), 'after'))
        server$set_data('passed_response', response)
    })
    response <- app$test_request(request)
    
    expect_equal(app$get_data('events'), c('before', 'during', 'after'))
    expect_equal(app$get_data('passed_args'), 4)
    expect_equal(app$get_data('passed_response'), response)
})

test_that('message events fire', {
    app <- Fire$new()
    request <- fake_request('http://www.example.com')
    
    app$on('before-message', function(server, ...) {
        server$set_data('events', c(server$get_data('events'), 'before'))
        list(test = 4, message = 'test2')
    })
    app$on('message', function(server, ...) {
        server$set_data('events', c(server$get_data('events'), 'during'))
        server$set_data('passed_args', list(...)[c('test', 'message')])
    })
    app$on('after-message', function(server, response, ...) {
        server$set_data('events', c(server$get_data('events'), 'after'))
    })
    app$test_message(request, FALSE, 'test')
    
    expect_equal(app$get_data('events'), c('before', 'during', 'after'))
    expect_equal(app$get_data('passed_args'), list(test = 4, message = 'test2'))
})

test_that('header event fire', {
    app <- Fire$new()
    request <- fake_request('http://www.example.com')
    
    app$on('header', function(server, ...) {
        server$set_data('header', TRUE)
    })
    app$test_header(request)
    expect_true(app$get_data('header'))
})

test_that('non-implemented methods are errors', {
    app <- Fire$new()
    
    expect_error(app$async())
    expect_error(app$time())
    expect_error(app$delay())
})

test_that('ignite is blocked during run', {
    app <- Fire$new()
    
    app$ignite(block = FALSE)
    expect_warning(app$ignite(), 'Server is already running and cannot be started')
    app$extinguish()
})

test_that('external triggers are fired', {
    app <- Fire$new()
    
    dir <- tempdir()
    app$triggerDir <- dir
    
    app$on('test', function(server, ...) {
        server$set_data('ext_args', list(...))
        server$extinguish()
    })
    saveRDS(4, file.path(dir, 'testfail.rds'))
    saveRDS(list(test = 'test'), file.path(dir, 'test.rds'))
    expect_warning(app$ignite(), 'External triggers must be an rds file containing a list')
    expect_equal(list(test = 'test'), app$get_data('ext_args'))
})

test_that('websockets are attached, and removed', {
    app <- Fire$new()
    
    app$on('send', function(server, ...) {server$set_data('send', TRUE)})
    expect_null(app$get_data('send'))
    expect_message(app$test_websocket(fake_request('http://www.example.com'), 'test'), 'test')
    expect_true(app$get_data('send'))
})

test_that('showcase opens a browser', {
    oldopt <- options(browser = function(url) message('Open browser'))
    
    app <- Fire$new()
    app$on('cycle-end', function(server, ...) server$extinguish())
    
    expect_message(app$ignite(showcase = TRUE), 'Open browser')
    expect_message(app$ignite(showcase = TRUE, block = FALSE), 'Open browser')
    app$extinguish()
    options(oldopt)
})
