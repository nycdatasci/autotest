# source('tests/testthat.R')

library(autotest)
error_code = c()

## test numerics ---------------------------------------------
test_that('', {
  expect_equal(1, 1L)
  expect_equal(1, 0.999999)
  expect_equal(1, 0.8, tolerance=0.2)
  expect_equal(1:10, 1:10)
  expect_equal(1 / 0, Inf)
  expect_equal(-1 / 0, -Inf)
  expect_equal(c(1, Inf), c(1, Inf))
})

error_code = c(
  error_code,
  'expect_equal(NA, 1)',
  'expect_equal(NULL, 1)',               # test NA/NULL
  'expect_equal(c(1, 1), 1)',            # test length
  'expect_equal(1:10, c(1:9, 1))',       # test values
  'expect_equal(c(1, Inf), c(1, -Inf))'  # test inf/-inf
)

## test character ---------------------------------------------
test_that('', {
  expect_equal('a', 'a')
  expect_equal('', '')
  expect_equal(letters, tolower(LETTERS))
})

error_code = c(
  error_code,
  'expect_equal(NA, "1")',
  'expect_equal(NULL, "1")',
  'expect_equal(c("1", "1"), "1")',
  'expect_equal(c("1", "1"), c("1", "2"))'
)

## test logical ---------------------------------------------
test_that('', {
  i = 10
  expect_equal(TRUE, TRUE)
  expect_equal(FALSE, FALSE)
  expect_equal(1:i >= 1, rep(TRUE, i))
  expect_equal(1:i < 1, rep(FALSE, i))
})

error_code = c(
  error_code,
  'expect_equal(NULL, TRUE)',
  'expect_equal(NULL, FALSE)',
  'expect_equal(NA, FALSE)',
  'expect_equal(NA, TRUE)'
)

## test factor ---------------------------------------------
test_that('test factor', {
  x = factor(1:10)
  expect_equal(x, factor(1:10))
  x = factor(1:10, levels=1:20)
  expect_equal(x, factor(1:10, levels=1:20))
  expect_equal(factor(), factor())
  expect_equal(factor(1:2, ordered = T), factor(1:2))
})

error_code = c(
  error_code,
  'expect_equal(NULL, factor())',
  'expect_equal(NA, factor())',
  'x=factor(1:5, levels=1:10); expect_equal(x, factor(1:5))',  # leverl not fit
  'expect_equal(factor(1:2), factor(1:2, ordered = T))'  # unordered
)

## test list ---------------------------------------------
test_that('test list', {
  x = list(a = iris, b = NULL, c=1:10)
  expect_equal(x, list(a = iris, b = NULL, c=1:10))
})

error_code = c(
  error_code,
  'x = list(a = iris, b = NULL, c=1:10); expect_equal(x, list(a = iris, b = NULL, c=1:11))',
  'x = list(a = iris, b = NULL, c=1:10); expect_equal(x, list(a = iris, b = NA, c=1:10))'
)

## test matrix ---------------------------------------------
test_that('test list', {
  x = matrix()
  expect_equal(x, matrix())
})

error_code = c(
  error_code,
  'x=matrix(1:9, 3); y=matrix(as.character(1:9), 3);expect_equal(x, y)',
  'x=matrix(1:9, 3); y=matrix(1:12, 3);expect_equal(x, y)',
  'x=matrix(1:9, 3); x[1,2]=6; y=matrix(1:12, 3);expect_equal(x, y)',
  'expect_equal(NULL, matrix())',
  'expect_equal(NA, matrix())'
)

## test data.frame ---------------------------------------------


## test error ---------------------------------------------
error_test <- function(error_code_vec){
  count = 0; mid_count = 0
  handle_msg = function(e) {
    mid_count <<- count + 1
    if (!startsWith(e$message, 'AutoTestCaseError')) {
      msg = sprintf('Here is the code: %s\n', code)
      cat('Unexpect error: ', e$message, '\n')
      stop(msg)
    }
    cat(paste(rep('-', 30), collapse = ''), '\n')
    cat(code, '\n\n')
    cat(e$message, '\n')
  }
  for (code in error_code_vec){
    tryCatch(
      eval(parse(text=code)),
      expectation = handle_msg,
      error = handle_msg,
      finally = {
        if (mid_count - count != 1){
          msg = sprintf('The code does not raise an error: %s', code)
          stop(msg)
        }
        count = mid_count
      }
    )
  }
}

error_test(error_code)


