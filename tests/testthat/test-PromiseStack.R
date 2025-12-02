test_that('DelayStack works', {
  rs <- r_session()
  rs(catcher <- new.env())
  rs(delay <- fiery:::DelayStack(app))
  expect_true(rs(delay$empty()))
  rs({
    delay$add(
      {
        message('test')
        10
      },
      function(res, ...) {
        catcher$res <- res + 10
      }
    )
  })
  expect_false(rs(delay$empty()))
  expect_snapshot(rs(delay$eval()))
  expect_equal(rs(catcher$res), 20)
  expect_true(rs(delay$empty()))

  rs(delay$add({
    message('test2')
  }))
  expect_snapshot(rs(delay$eval()))

  expect_true(rs(delay$empty()))
  rs(id <- delay$add(10))
  expect_false(rs(delay$empty()))
  rs(delay$remove(id))
  expect_true(rs(delay$empty()))

  rs(delay$add(stop('error test')))
  rs(delay$add(5, function(res, ...) catcher$res <- res))
  expect_snapshot(rs(delay$eval()))
  expect_equal(rs(catcher$res), 5)

  rs(delay$add(NULL, function(...) stop('error test')))
  rs(delay$add(15, function(res, ...) catcher$res <- res))
  expect_snapshot(rs(delay$eval()))
  expect_equal(rs(catcher$res), 15)
})

test_that('TimeStack works', {
  rs <- r_session()
  rs(catcher <- new.env())
  rs(time <- fiery:::TimeStack(app))
  expect_true(rs(time$empty()))
  rs({
    time$add(
      {
        message('test')
        10
      },
      function(res, ...) {
        catcher$res <- res + 10
      },
      after = 2
    )
  })
  expect_false(rs(time$empty()))
  expect_silent(rs(time$eval()))
  rs(Sys.sleep(3))
  expect_snapshot(rs(time$eval()))
  expect_equal(rs(catcher$res), 20)
  expect_true(rs(time$empty()))

  rs({
    time$add(
      {
        message('test')
        10
      },
      function(res, ...) {
        catcher$res <- res + 20
      },
      after = 2
    )
  })
  rs(Sys.sleep(3))
  rs(time$reset())
  rs(time$eval())
  rs(Sys.sleep(3))
  expect_snapshot(rs(time$eval()))
  expect_equal(rs(catcher$res), 30)
  expect_true(rs(time$empty()))

  rs({
    id <- time$add(
      {
        message('test')
        10
      },
      function(res, ...) {
        catcher$res <- res + 30
      },
      after = 2,
      loop = TRUE
    )
  })
  expect_silent(rs(time$eval()))
  rs(Sys.sleep(3))
  expect_snapshot(rs(time$eval()))
  expect_false(rs(time$empty()))
  expect_equal(rs(catcher$res), 40)
  expect_silent(rs(time$eval()))
  rs(Sys.sleep(3))
  expect_snapshot(rs(time$eval()))
  rs(time$remove(id))

  expect_true(rs(time$empty()))
  rs(id <- time$add(10, after = 1))
  expect_false(rs(time$empty()))
  rs(time$remove(id))
  expect_true(rs(time$empty()))
})
