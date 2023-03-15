fun1 <- function(...) {
    length(list(...))
}
fun2 <- function(...) {
    ..1
}

test_that("Handlers can be added and removed", {
    stack <- HandlerStack$new()
    fun1 <- function(...) {
        length(list(...))
    }
    fun2 <- function(...) {
        ..1
    }
    stack$add(fun1, 'a')
    stack$add(fun2, 'b')
    expect_equal(stack$length(), 2)
    expect_true(stack$contains('a'))
    expect_true(stack$contains('b'))
    expect_false(stack$contains('c'))
    expect_equal(stack$position('b'), 2)
    expect_equal(stack$position(c('a', 'c')), c(1, NA_integer_))
    
    fun1return <- stack$remove('a')
    expect_equal(stack$length(), 1)
    expect_false(stack$contains('a'))
    expect_true(stack$contains('b'))
    expect_equal(stack$position('b'), 1)
    expect_equal(stack$position(c('a', 'c')), c(NA_integer_, NA_integer_))
    expect_equal(fun1return, fun1)
    
    fun2return <- stack$remove('b')
    expect_equal(stack$length(), 0)
    expect_false(stack$contains('a'))
    expect_false(stack$contains('b'))
    expect_equal(stack$position('b'), NA_integer_)
    expect_equal(fun2return, fun2)
})

test_that('Inserting at position works', {
    stack <- HandlerStack$new()
    stack$add(length, 'length')
    stack$add(str, 'str')
    stack$add(sum, 'sum')
    
    stack$add(min, 'min', 1)
    expect_equal(stack$position('min'), 1)
    
    stack$add(max, 'max', 100)
    expect_equal(stack$position('max'), 5)
    
    stack$add(mean, 'mean', 3)
    expect_equal(stack$position('mean'), 3)
})

test_that('Assertions throw errors', {
    stack <- HandlerStack$new()
    expect_error(stack$add('string', 'id'))
    expect_error(stack$add(c(min, max), 'id'))
    expect_error(stack$add(min, 3))
    expect_error(stack$add(min, 3:5))
    expect_error(stack$add(min, 'min', 'position'))
    expect_error(stack$add(min, 'min', 1:4))
    expect_error(stack(add(min, 'min', 1.5)))
    expect_error(stack$remove(1))
    expect_error(stack$remove(c('1', '2')))
    expect_error(stack$position(1))
    expect_error(stack$contains(1))
})

test_that('Dispatch works', {
    stack <- HandlerStack$new()
    args <- 1:10
    expect_type(stack$dispatch(args), 'list')
    expect_length(stack$dispatch(args), 0)
    expect_named(stack$dispatch(args), character())
    
    stack$add(min, 'min')
    stack$add(mean, 'mean')
    stack$add(max, 'max')
    
    expect_type(stack$dispatch(args), 'list')
    expect_length(stack$dispatch(args), 3)
    expect_named(stack$dispatch(args), c('min', 'mean', 'max'))
    expect_equal(stack$dispatch(args), list(min = min(args), mean = mean(args), max = max(args)))
})