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
