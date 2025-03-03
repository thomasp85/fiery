app <- Fire$new(port = random_port())

test_that('DelayStack works', {
    catcher <- new.env()
    delay <- DelayStack$new(app)
    expect_true(delay$empty())
    delay$add({
        message('test')
        10
    }, function(res, ...) {
        catcher$res <- res + 10
    })
    expect_false(delay$empty())
    expect_snapshot(delay$eval())
    expect_equal(catcher$res, 20)
    expect_true(delay$empty())

    delay$add({
        message('test2')
    })
    expect_snapshot(delay$eval())

    expect_true(delay$empty())
    id <- delay$add(10)
    expect_false(delay$empty())
    delay$remove(id)
    expect_true(delay$empty())

    delay$add(stop('error test'))
    delay$add(5, function(res, ...) catcher$res <- res)
    expect_snapshot(delay$eval())
    expect_equal(catcher$res, 5)

    delay$add(NULL, function(...) stop('error test'))
    delay$add(15, function(res, ...) catcher$res <- res)
    expect_snapshot(delay$eval())
    expect_equal(catcher$res, 15)
})

test_that('TimeStack works', {
    catcher <- new.env()
    time <- TimeStack$new(app)
    expect_true(time$empty())
    time$add({
        message('test')
        10
    }, function(res, ...) {
        catcher$res <- res + 10
    }, after = 2)
    expect_false(time$empty())
    expect_silent(time$eval())
    Sys.sleep(3)
    expect_snapshot(time$eval())
    expect_equal(catcher$res, 20)
    expect_true(time$empty())

    time$add({
        message('test')
        10
    }, function(res, ...) {
        catcher$res <- res + 20
    }, after = 2)
    Sys.sleep(3)
    time$reset()
    expect_silent(time$eval())
    Sys.sleep(3)
    expect_snapshot(time$eval())
    expect_equal(catcher$res, 30)
    expect_true(time$empty())

    id <- time$add({
        message('test')
        10
    }, function(res, ...) {
        catcher$res <- res + 30
    }, after = 2, loop = TRUE)
    expect_silent(time$eval())
    Sys.sleep(3)
    expect_snapshot(time$eval())
    expect_false(time$empty())
    expect_equal(catcher$res, 40)
    expect_silent(time$eval())
    Sys.sleep(3)
    expect_snapshot(time$eval())
    time$remove(id)

    expect_true(time$empty())
    id <- time$add(10, after = 1)
    expect_false(time$empty())
    time$remove(id)
    expect_true(time$empty())
})
