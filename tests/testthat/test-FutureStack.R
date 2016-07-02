context("FutureStack")

test_that('DelayStack works', {
    catcher <- new.env()
    delay <- DelayStack$new()
    expect_true(delay$empty())
    delay$add({
        message('test')
        10
    }, function(res, ...) {
        catcher$res <- res + 10
    })
    expect_false(delay$empty())
    expect_message(delay$eval(), 'test')
    expect_equal(catcher$res, 20)
    expect_true(delay$empty())
    
    delay$add({
        message('test2')
    })
    expect_message(delay$eval(), 'test2')
    
    expect_true(delay$empty())
    id <- delay$add(10)
    expect_false(delay$empty())
    delay$remove(id)
    expect_true(delay$empty())
})

test_that('TimeStack works', {
    catcher <- new.env()
    time <- TimeStack$new()
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
    expect_message(time$eval(), 'test')
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
    expect_message(time$eval(), 'test')
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
    expect_message(time$eval(), 'test')
    expect_false(time$empty())
    expect_equal(catcher$res, 40)
    expect_silent(time$eval())
    Sys.sleep(3)
    expect_message(time$eval(), 'test')
    time$remove(id)
    
    expect_true(time$empty())
    id <- time$add(10, after = 1)
    expect_false(time$empty())
    time$remove(id)
    expect_true(time$empty())
})