context("loggers")

test_that("null logger works", {
    logger <- logger_null()
    expect_equal(capture.output(logger('error', 'error test'), type = 'message'),
                 "error: error test")
    expect_equal(capture.output(logger('warning', 'warning test'), type = 'message'),
                 "warning: warning test")
    expect_equal(capture.output(logger('info', 'info test'), type = 'message'),
                 character(0))
})

test_that("console logger works", {
    logger <- logger_console()
    expect_output(logger('error', 'error test'), "error: error test")
    expect_output(logger('warning', 'warning test'), "warning: warning test")
    expect_output(logger('info', 'info test'), 'info: info test')
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
