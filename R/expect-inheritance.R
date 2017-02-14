#' Expectation: does the object inherit from a class, or a base type
#'
#' Tests whether or not an object inherits from any of a list of classes, or
#' is an instance of a base type.
#'
#' `expect_is()` is an older form. I'd recommend using `expect_s3_class()`
#' or `expect_s4_class()` in order to more clearly convey intent.
#'
#' @inheritParams expect_that
#' @seealso [inherits()]
#' @family expectations
#' @examples
#' expect_is(1, "numeric")
#' a <- matrix(1:10, nrow = 5)
#' expect_is(a, "matrix")
#'
#' expect_is(mtcars, "data.frame")
#' # alternatively for classes that have an is method
#' expect_true(is.data.frame(mtcars))
#'
#' f <- factor("a")
#' expect_is(f, "factor")
#'
#' @name inheritance-expectations
NULL


#' @param class character vector of class names
#' @param type String giving base type (as returned by [typeof()]).
#' @export
#' @rdname inheritance-expectations
expect_is <- function(object, class, info = NULL, label = NULL, trace=TRUE) {
  # lab <- make_label(object, label)
  lab <- deparse(substitute(object))
  stopifnot(is.character(class))
  if (ErrorHandler$trace && trace){
    ErrorHandler$setTesting("Testing variable/expression:  %s", lab)
  }

  act <- klass(object)
  exp <- paste(class, collapse = "/")

  expect(
    inherits(object, class),
    sprintf("%s is a/an `%s` object, it should be a `%s` object.", lab, act, exp),
    info = info
  )
  invisible(object)
}

