tryCatch(source('error.R'),
         error = function(e) source('tests/testthat/error.R'),
         warning = function(e) invisible()
)
library(autotest)

## normal code ------------------------
test_that('testing true', {
  expect_true(TRUE)
  expect_true(rep(TRUE, 10))
})

test_that('testing false', {
  expect_false(FALSE)
  expect_false(rep(FALSE, 10))
})

## error code ------------------------
error_code = c(
  'expect_true(NULL)',
  'expect_true(NA)',
  'expect_false(NA)',
  'expect_false(NULL)'
)
error_test(error_code)
