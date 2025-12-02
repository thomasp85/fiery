test_that("null logger works", {
  rs <- r_session()
  rs(logger <- logger_null())
  expect_snapshot(rs(logger('error', error_cnd(message = 'error test'))))
  expect_snapshot(rs(logger('warning', warning_cnd(message = 'warning test'))))
  expect_snapshot(rs(logger('message', message_cnd(message = 'message test'))))
  expect_silent(rs(logger('info', 'info test')))
})

test_that("console logger works", {
  rs <- r_session()
  rs(logger <- logger_console("{event}: {message}"))
  rs(req <- reqres::Request$new(fake_request('www.example.com')))
  expect_snapshot(rs(logger('error', 'error test')))
  expect_snapshot(rs(logger('warning', 'warning test')))
  expect_snapshot(rs(logger('info', 'info test')))
  expect_snapshot(rs(logger('request', 'request test', req)))
  expect_snapshot(rs(logger('error', catch_cnd(stop("error test")))))
  expect_snapshot(rs(logger('warning', catch_cnd(warning("warning test")))))
  expect_snapshot(rs(logger('message', catch_cnd(message("message test")))))
  expect_snapshot(rs(logger(
    'info',
    catch_cnd(cnd("info", message = "info test"))
  )))
  expect_snapshot(rs(logger(
    'error',
    catch_cnd(reqres::abort_bad_request("http problem", instance = "test"))
  )))
})

test_that("file logger works", {
  rs <- r_session()
  rs(logfile <- tempfile())
  rs(logger <- logger_file(logfile))
  rs(logger('error', 'error test'))
  rs(logger('warning', 'warning test'))
  rs(logger('info', 'info test'))
  logs <- rs(readLines(logfile))
  expect_match(logs[1], "error: error test")
  expect_match(logs[2], "warning: warning test")
  expect_match(logs[3], 'info: info test')
})

test_that("switch logger works", {
  rs <- r_session()
  rs(logfile <- tempfile())
  rs({
    logger <- logger_switch(
      test = ,
      info = logger_console("{event}: {message}"),
      error = logger_file(logfile),
      default = logger_null()
    )
  })
  expect_snapshot(rs(logger('test', 'test test')))
  expect_snapshot(rs(logger('info', 'info test')))
  expect_snapshot(rs(logger('warning', warning_cnd(message = 'warning test'))))
  rs(logger('error', 'error test'))
  logs <- rs(readLines(logfile))
  expect_match(logs[1], "error: error test")
})

test_that("logger logger works", {
  skip_if_not_installed("logger")
  rs <- r_session()
  rs(logger <- logger_logger())
  rs({
    logger::log_layout(logger::layout_glue_generator(
      '{level} [2025-02-26 08:18:18] {msg}}'
    ))
  })
  rs(req <- reqres::Request$new(fake_request('www.example.com')))
  expect_snapshot(rs(logger('error', 'error test')))
  expect_snapshot(rs(logger('warning', 'warning test')))
  expect_snapshot(rs(logger('info', 'info test')))
  expect_snapshot(rs(logger('request', 'request test', req)))
  expect_snapshot(rs(logger(logger::INFO, 'info test')))
  expect_snapshot(rs(logger(1, 'info test')))
})
