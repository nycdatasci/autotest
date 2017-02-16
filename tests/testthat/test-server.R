library(autotest)
test_that('', {
  x = console::run_single('autotest::test_that("", autotest::expect_equal(1, 2))')
  x = x[[1]]
  expect_is(x, 'list')
  expect_equal(x$type, 'error')
  expect_match(x$string, 'AutoTestCaseError')
})
