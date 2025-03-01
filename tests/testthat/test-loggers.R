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
    expect_snapshot(logger('error', catch_cnd(stop("error test"))))
    expect_snapshot(logger('warning', catch_cnd(warning("warning test"))))
    expect_snapshot(logger('message', catch_cnd(message("message test"))))
    expect_snapshot(logger('info', catch_cnd(cnd("info", message = "info test"))))
    expect_snapshot(logger('error', catch_cnd(reqres::abort_bad_request("http problem", instance = "test"))))
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

test_that("logger logger works", {
    skip_on_cran()
    skip_if_not_installed("logger")
    logger <- logger_logger()
    logger::log_layout(logger::layout_glue_generator(
        '{level} [2025-02-26 08:18:18] {msg}}'
    ))
    req <- reqres::Request$new(fake_request('www.example.com'))
    expect_snapshot(logger('error', 'error test'))
    expect_snapshot(logger('warning', 'warning test'))
    expect_snapshot(logger('info', 'info test'))
    expect_snapshot(logger('request', 'request test', req))
    expect_snapshot(logger(logger::INFO, 'info test'))
    expect_snapshot(logger(1, 'info test'))
})
