#' Expectation: is the object true/false?
#'
#' These are fall-back expectations that you can use when none of the other
#' more specific expectations apply. The disadvantage is that you may get
#' a less informative error message.
#'
#' Attributes are ignored.
#'
#' @seealso [is_false()] for complement
#' @inheritParams expect_that
#' @family expectations
#' @examples
#' expect_true(2 == 2)
#' # Failed expectations will throw an error
#' \dontrun{
#' expect_true(2 != 2)
#' }
#' expect_true(!(2 != 2))
#' # or better:
#' expect_false(2 != 2)
#'
#' a <- 1:3
#' expect_true(length(a) == 3)
#' # but better to use more specific expectation, if available
#' expect_equal(length(a), 3)
#' @name logical-expectations
NULL

#' @export
#' @rdname logical-expectations
expect_true <- function(object, info = NULL, label = NULL,
                        trace=TRUE, suppressErr=FALSE) {
  lab <- deparse(substitute(object))
  if (ErrorHandler$trace && trace){
    ErrorHandler$setTesting("Testing variable/expression:  %s", lab)
  }
  if (class(object) != 'logical'){
    msg = sprintf("The type of your answer is %s, the type should be logical", class(object))
    is_expect = FALSE
  }else{
    is_expect = identical(as.vector(object), rep(TRUE, length(object)))
    msg = ifelse(length(object) == 1,
                 sprintf("Your answer is %s, it should be TRUE", object),
                 "All the elements of your answer should be exactly TRUE.")
  }
  expect(
    is_expect,
    ifelse(suppressErr, '', msg),
    info = info
  )
  invisible(object)
}

#' @export
#' @rdname logical-expectations
expect_false <- function(object, info = NULL, label = NULL,
                         trace=TRUE, suppressErr=FALSE) {
  lab <- deparse(substitute(object))
  if (ErrorHandler$trace && trace){
    ErrorHandler$setTesting("Testing variable/expression:  %s", lab)
  }
  if (class(object) != 'logical'){
    is_expect = FALSE
    msg = sprintf("The type of your answer is %s, it should be logical", class(object))

  }else{
    is_expect = identical(as.vector(object), rep(FALSE, length(object)))
    msg = ifelse(length(object) == 1,
                 sprintf("Your answer is %s, it should be FALSE", object),
                 "All the elements of your answer should be exactly FALSE.")
  }
  expect(is_expect,
         ifelse(suppressErr, '', msg),
         info = info)
  invisible(object)
}

#' @export
#' @rdname logical-expectations
expect_na <- function(object, info=NULL, label = NULL, method='all',
                      trace=TRUE, suppressErr=FALSE){
  lab <- deparse(substitute(object))
  if (ErrorHandler$trace && trace){
    ErrorHandler$setTesting("Testing variable/expression:  %s", lab)
  }
  res = ifelse(method == 'all', all(is.na(object)), any(is.na(object)))
  msg = sprintf('We expect your answer to be NA, however it is/are not')
  expect(res,
         ifelse(suppressErr, '', msg),
         info=info)
  invisible(object)
}

