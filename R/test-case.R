#' Create a test case.
#'
#' A test case is a function that tests a series of expectations about
#' small, self-contained set of functionality. Each test is contained in a
#' \link{context} and contains multiple expectations.
#'
#' @param code test code containing expectations
#' @return A test_that function containing the test code
#' @export
#' @examples
#' case_1 <- test_case({
#'   expect_equal(sin(pi / 4), 1 / sqrt(2))
#'   expect_equal(cos(pi / 4), 1 / sqrt(2))
#'   expect_equal(tan(pi / 4), 1)
#' })
#' case_1()
#' # Failing test:
#' \dontrun{
#' case_2 <- test_case(expect_equal(sin(pi / 4), 1))
#' case_2()
#' }
test_case <- function(code) {
  function(desc = "") {
    test_that(desc = desc, code = code)
  }
}

#' Run test cases.
#'
#' The `runTest()` function should be used to run test cases you defined
#' and it will generate output message if all tests have been passed.
#'
#' @return A string of the format "AutoTest cases passed: {number of test cases}"
#' @export
runTest <- function(...) {
  cases <- c(...)
  for(case in cases) {
    case()
  }
  return(paste("AutoTest cases passed:", length(cases)))
}
