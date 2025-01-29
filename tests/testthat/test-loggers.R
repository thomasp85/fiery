test_that("null logger works", {
    logger <- logger_null()
    expect_snapshot(logger('error', 'error test'))
    expect_snapshot(logger('warning', 'warning test'))
    expect_snapshot(logger('message', 'message test'))
    expect_silent(logger('info', 'info test'))
})

test_that("console logger works", {
    skip_on_cran()
    logger <- logger_console("{event}: {message}")
    req <- reqres::Request$new(fake_request('www.example.com'))
    expect_snapshot(logger('error', 'error test'))
    expect_snapshot(logger('warning', 'warning test'))
    expect_snapshot(logger('info', 'info test'))
    expect_snapshot(logger('request', 'request test', req))
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
    logger <- logger_switch(test = , info = logger_console("{event}: {message}"),
                            error = logger_file(logfile),
                            default = logger_null())
    expect_snapshot(logger('test', 'test test'))
    expect_snapshot(logger('info', 'info test'))
    expect_snapshot(logger('warning', 'warning test'))
    logger('error', 'error test')
    logs <- readLines(logfile)
    expect_match(logs[1], "error: error test")
})

test_that("%||% works", {
    expect_equal(NULL %||% 10, 10)
    expect_equal(20 %||% 10, 20)
})
