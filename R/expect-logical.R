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
expect_true <- function(object, info = NULL, label = NULL, trace=TRUE) {
  # lab <- make_label(object, label)
  lab <- deparse(substitute(object))
  if (ErrorHandler$trace && trace){
    pre_msg = sprintf("Testing variable/expression:  %s",
                      deparse(substitute(object)))
    ErrorHandler$setTesting(pre_msg)
  }
  if (class(object) != 'logical'){
    expect(FALSE,
           sprintf("The type of your answer is %s, the type should be logical", class(object)),
           info = info)
  }else{
    expect(
      identical(as.vector(object), rep(TRUE, length(object))),
      ifelse(length(object) == 1,
             sprintf("Your answer is %s, the type should be TRUE", object),
             sprintf("All the elements of your answer should be exactly TRUE.")
      ),
      info = info
    )
  }
  invisible(object)
}

#' @export
#' @rdname logical-expectations
expect_false <- function(object, info = NULL, label = NULL, trace=TRUE) {
  # lab <- make_label(object, label)
  lab <- deparse(substitute(object))
  if (ErrorHandler$trace && trace){
    pre_msg = sprintf("Testing variable/expression:  %s", lab)
    ErrorHandler$setTesting(pre_msg)
  }
  if (class(object) != 'logical'){
    expect(FALSE,
           sprintf("The type of your answer is %s, it should be logical", class(object)),
            info = info)
  }else{
    expect(
      identical(as.vector(object), rep(FALSE, length(object))),
      ifelse(length(object) == 1,
             sprintf("Your answer is %s, it should be FALSE", object),
             sprintf("All the elements of your answer should be exactly FALSE.")
             ),
      info = info
    )
  }
  invisible(object)
}

#' @export
#' @rdname logical-expectations
expect_na <- function(object, info=NULL, label = NULL, method='all', trace=TRUE){
  lab <- deparse(substitute(object))
  if (ErrorHandler$trace && trace){
    pre_msg = sprintf("Testing variable/expression:  %s", lab)
    ErrorHandler$setTesting(pre_msg)
  }
  res = ifelse(method == 'all', all(is.na(object)), any(is.na(object)))
  expect(res,
         sprintf('We expect your answer to be NA, however it is/are not'),
         info=info)
  invisible(object)
}

