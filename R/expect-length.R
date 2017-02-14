#' Expectation: does a vector have the specified length?
#'
#' @inheritParams expect_that
#' @param n Expected length.
#' @family expectations
#' @export
#' @examples
#' expect_length(1, 1)
#' expect_length(1:10, 10)
#'
#' \dontrun{
#' expect_length(1:10, 1)
#' }
expect_length <- function(object, n, trace=TRUE) {
  lab <- deparse(substitute(object))
  stopifnot(is.numeric(n), length(n) == 1)
  if (ErrorHandler$trace && trace){
    ErrorHandler$setTesting("Testing variable/expression:  %s", lab)
  }

  if (!is_vector(object)) {
    fail(sprintf("%s is not a vector.", lab))
  }

  expect(
    length(object) == n,
    sprintf("The length of %s is %s, it should be %s.", lab, length(object), n)
  )

  invisible(object)
}

is_vector <- function(x) {
  typeof(x) %in% c("logical", "integer", "double", "complex", "character", "raw", "list")
}
