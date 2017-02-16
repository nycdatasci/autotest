#' Expectation: does string/output/message/warning/error match a regular expression?
#'
#' @inheritParams expect_that
#' @param regexp Regular expression to test against.
#' @param all Should all elements of actual value match `regexp` (TRUE),
#'    or does only one need to match (FALSE)
#' @param ... Additional arguments passed on to [grepl()], e.g.
#'   `ignore.case` or `fixed`.
#' @family expectations
#' @export
#' @examples
#' expect_match("Testing is fun", "fun")
#' expect_match("Testing is fun", "f.n")
#'
#' \dontrun{
#' expect_match("Testing is fun", "horrible")
#'
#' # Zero-length inputs always fail
#' expect_match(character(), ".")
#' }
expect_match <- function(object, regexp, ..., all = TRUE,
                         info = NULL, label = NULL,
                         trace=TRUE, suppressErr=FALSE) {

  label <- deparse(substitute(object))
  if (ErrorHandler$trace && trace){
    ErrorHandler$setTesting("Testing variable/expression:  %s", label)
  }
  stopifnot(is.character(regexp), length(regexp) == 1)

  stopifnot(is.character(object))
  if (length(object) == 0) {
    fail(sprintf("%s is empty.", label))
  }

  matches <- grepl(regexp, object, ...)

  # if (length(object) == 1) {
  #   values <- paste0("Actual value: \"", escape_regex(encodeString(object)), "\"")
  # } else {
  #   values <- paste0("Actual values:\n",
  #     paste0("* ", escape_regex(encodeString(object)), collapse = "\n"))
  # }
  msg = sprintf(
    "%s does not match %s.",
    label, # escape_regex(label),
    encodeString(regexp, quote = '"')
  )
  expect(
    if (all) all(matches) else any(matches),
    ifelse(suppressErr, '', msg),
    info = info
  )
  invisible(object)
}
