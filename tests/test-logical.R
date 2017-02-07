library(autotest)

## normal code
test_that('testing true', {
  expect_true(TRUE)
  expect_true(rep(TRUE, 10))
})

test_that('testing false', {
  expect_false(FALSE)
  expect_false(rep(FALSE, 10))
})

## error code
error_code = c(
  'expect_true(NULL)',
  'expect_true(NA)',
  'expect_false(NA)',
  'expect_false(NULL)'
)
handle_msg = function(e) stopifnot(startsWith(e$message, 'AutoTestCaseError'))
test_that('test error message', {
  for (code in error_code){
    tryCatch(
      expect_true(NULL),
      expectation = handle_msg,
      error = handle_msg
    )
  }
})
