context("loggers")

test_that("null logger works", {
    logger <- logger_null()
    expect_message(logger('error', 'error test'), "error: error test")
    expect_message(logger('warning', 'warning test'), "warning: warning test")
    expect_message(logger('message', 'message test'), "message: message test")
    expect_silent(logger('info', 'info test'))
})

test_that("console logger works", {
    logger <- logger_console()
    req <- reqres::Request$new(fake_request('www.example.com'))
    expect_output(logger('error', 'error test'), "error: error test")
    expect_output(logger('warning', 'warning test'), "warning: warning test")
    expect_output(logger('info', 'info test'), 'info: info test')
    expect_output(logger('request', 'request test', req), 'request: request test')
})

test_that("file logger works", {
    logfile <- tempfile()
    logger <- logger_file(logfile)
    logger('error', 'error test')
    logger('warning', 'warning test')
    logger('info', 'info test')
    logs <- readLines(logfile)
    expect_match(logs[1], "error: error test")
    expect_match(logs[2], "warning: warning test")
    expect_match(logs[3], 'info: info test')
})

test_that("switch logger works", {
    logfile <- tempfile()
    logger <- logger_switch(test = , info = logger_console(), 
                            error = logger_file(logfile),
                            default = logger_null())
    expect_output(logger('test', 'test test'), "test: test test")
    expect_output(logger('info', 'info test'), "info: info test")
    expect_message(logger('warning', 'warning test'), "warning: warning test")
    logger('error', 'error test')
    logs <- readLines(logfile)
    expect_match(logs[1], "error: error test")
})

test_that("%||% works", {
    expect_equal(NULL %||% 10, 10)
    expect_equal(20 %||% 10, 20)
})
