#' Expectation: does code produce output/message/warning/error?
#'
#' Use `expect_output()`, `expect_message()`, `expect_warning()`,
#' or `expect_error()` to check for specific outputs. Use
#' `expect_silent()` to assert that there should be no output of
#' any type. The file-based`expect_output_file()` compares the output
#' to the contents of a text file and optionally updates it.
#'
#' Note that warnings are captured by a custom signal handler: this means
#' that `options(warn)` has no effect.
#'
#' @inheritParams expect_that
#' @inheritParams expect_match
#' @param regexp regular expression to test against.
#'
#'   If `NULL`, the default, asserts that there should be an output,
#'   a messsage, a warning, or an error, but does not test for specific value.
#'
#'   If `NA`, asserts that there should be no output, messages, warnings,
#'   or errors.
#' @param class Instead of supply a regular expression, you can also supply
#'   a class name. This is useful for "classed" conditions.
#' @param all For messages and warnings, do all need to the `regexp`
#'    (TRUE), or does only one need to match (FALSE)
#' @family expectations
#' @examples
#' # Output --------------------------------------------------------------------
#' str(mtcars)
#' expect_output(str(mtcars), "32 obs")
#' expect_output(str(mtcars), "11 variables")
#'
#' # You can use the arguments of grepl to control the matching
#' expect_output(str(mtcars), "11 VARIABLES", ignore.case = TRUE)
#' expect_output(str(mtcars), "$ mpg", fixed = TRUE)
#'
#' # Messages ------------------------------------------------------------------
#'
#' f <- function(x) {
#'   if (x < 0) message("*x* is already negative")
#'   -x
#' }
#' expect_message(f(-1))
#' expect_message(f(-1), "already negative")
#' expect_message(f(1), NA)
#'
#' # You can use the arguments of grepl to control the matching
#' expect_message(f(-1), "*x*", fixed = TRUE)
#' expect_message(f(-1), "NEGATIVE", ignore.case = TRUE)
#'
#' # Warnings --------------------------------------------------------------------
#' f <- function(x) {
#'   if (x < 0) warning("*x* is already negative")
#'   -x
#' }
#' expect_warning(f(-1))
#' expect_warning(f(-1), "already negative")
#' expect_warning(f(1), NA)
#'
#' # You can use the arguments of grepl to control the matching
#' expect_warning(f(-1), "*x*", fixed = TRUE)
#' expect_warning(f(-1), "NEGATIVE", ignore.case = TRUE)
#'
#' # Errors --------------------------------------------------------------------
#' f <- function() stop("My error!")
#' expect_error(f())
#' expect_error(f(), "My error!")
#'
#' # You can use the arguments of grepl to control the matching
#' expect_error(f(), "my error!", ignore.case = TRUE)
#'
#' # Silent --------------------------------------------------------------------
#' expect_silent("123")
#'
#' f <- function() {
#'   message("Hi!")
#'   warning("Hey!!")
#'   print("OY!!!")
#' }
#' \dontrun{
#' expect_silent(f())
#' }

#' @name output-expectations
NULL

#' @export
#' @rdname output-expectations
expect_output <- function(object, regexp = NULL, ...,
                          info = NULL, label = NULL,
                          trace=TRUE, suppressErr=FALSE) {

  # lab <- make_label(object, label)
  lab <- deparse(substitute(object))
  if (ErrorHandler$trace && trace){
    ErrorHandler$setTesting("Testing variable/expression:  %s", lab)
  }
  output <- capture_output(object)

  if (identical(regexp, NA)) {
    msg = sprintf("%s produced output:\n%s", lab, encodeString(output))
    expect(
      identical(output, ""),
      ifelse(suppressErr, '', msg),
      info = info
    )
  } else if (is.null(regexp) || identical(output, "")) {
    expect(
      !identical(output, ""),
      ifelse(suppressErr, '', sprintf("%s produced no output", lab)),
      info = info
    )
  } else {
    msg = sprintf("%s produced output: %s. \nThe output shoud match: %s",
                  lab,
                  encodeString(output, quote = '"'),
                  encodeString(regexp, quote = '"'))
    expect(
      all(grepl(regexp, output, ...)),
      ifelse(suppressErr, '', msg),
      info = info
    )
  }
  invisible(NULL)
}


#' @export
#' @rdname output-expectations
#' @param file Path to a "golden" text file that contains the desired output.
#' @param update Should the "golden" text file be updated? Defaults to
#'   `FALSE`, but that it will automatically create output if the file
#'   does not exist (i.e. on the first run).
#' @inheritParams capture_output
expect_output_file <- function(object, file, update = FALSE, ...,
                               info = NULL, label = NULL, width = 80) {
  lab <- make_label(object, label)

  output <- capture_output_lines(object, print = FALSE, width = width)
  if (!file.exists(file)) {
    writeLines(output, file)
  }

  expr <- bquote(
    expect_equal(output, safe_read_lines(.(file)), ..., info = info, label = lab)
  )
  withCallingHandlers(
    eval(expr),
    expectation = function(e) {
      if (update && expectation_failure(e)) {
        tryCatch(writeLines(output, file), error = function(e) NULL)
      }
    }
  )
}


#' @export
#' @rdname output-expectations
expect_error <- function(object, regexp = NULL, class = NULL, ..., info = NULL,
                         label = NULL, trace=TRUE, suppressErr=FALSE) {
  # lab <- make_label(object, label)
  lab <- deparse(substitute(object))
  if (ErrorHandler$trace && trace){
    ErrorHandler$setTesting("Testing variable/expression:  %s", lab)
  }
  if (!is.null(regexp) && !is.null(class)) {
    stop("You may only specific one of `regexp` and `class`", call. = FALSE)
  }

  error <- tryCatch(
    {
      object
      NULL
    },
    error = function(e) {
      e
    }
  )

  if (!is.null(class)) {
    expect(
      inherits(error, class),
      ifelse(suppressErr, '',
        sprintf("The expression %s did not throw an error of class '%s'.", lab, class)),
      info = info
    )
  } else if (identical(regexp, NA)) {
    expect(
      is.null(error),
      ifelse(suppressErr, '',
        sprintf("The expression %s threw an error:\n%s", lab, error$message)),
      info = info
    )
  } else if (is.null(regexp) || is.null(error)) {
    expect(
      !is.null(error),
      ifelse(suppressErr, '',
        sprintf("The expression %s should throw an error, however it did not.", lab)),
      info = info
    )
  } else {
    expect(
      all(grepl(regexp, error$message, ...)),
      ifelse(suppressErr, '',
        sprintf("The expression %s throw an error: %s. \nThe error message shoud match: %s",
              lab,
              encodeString(error$message, quote = '"'),
              encodeString(regexp, quote = '"'))
        ),
      info = info
    )
  }
  invisible(NULL)
}

#' @export
#' @rdname output-expectations
expect_message <- function(object, regexp = NULL, ..., all = FALSE,
                           info = NULL, label = NULL) {

  lab <- make_label(object, label)
  messages <- capture_messages(object)
  n <- length(messages)
  # bullets <- paste("* ", messages, collapse = "\n")
  bullets <- paste(messages, collapse = "\n")


  msg <- sprintf(ngettext(n, "%d message", "%d messages"), n)

  if (identical(regexp, NA)) {
    expect(
      length(messages) == 0,
      sprintf("%s showed %s.\n%s", lab, msg, bullets),
      info = info
    )
  } else if (is.null(regexp) || length(messages) == 0) {
    expect(
      length(messages) > 0,
      sprintf("%s showed %s", lab, msg),
      info = info
    )
  } else {
    expect_match(messages, regexp, all = all, ..., info = info)
  }
  invisible(NULL)
}

#' @export
#' @rdname output-expectations
expect_warning <- function(object, regexp = NULL, ..., all = FALSE,
                           info = NULL, label = NULL) {

  lab <- make_label(object, label)
  warnings <- capture_warnings(object)
  n <- length(warnings)
  bullets <- paste("* ", warnings, collapse = "\n")

  msg <- sprintf(ngettext(n, "%d warning", "%d warnings"), n)

  if (identical(regexp, NA)) {
    expect(
      length(warnings) == 0,
      sprintf("%s showed %s.\n%s", lab, msg, bullets),
      info = info
    )
  } else if (is.null(regexp) || length(warnings) == 0) {
    expect(
      length(warnings) > 0,
      sprintf("%s showed %s", lab, msg),
      info = info
    )
  } else {
    expect_match(warnings, regexp, all = all, ...,info = info)
  }
  invisible(NULL)
}

#' @export
#' @rdname output-expectations
expect_silent <- function(object) {
  lab <- label(object)
  out <- evaluate_promise(object)

  outputs <- c(
    if (!identical(out$output, "")) "output",
    if (length(out$warnings) > 0) "warnings",
    if (length(out$messages) > 0) "messages"
  )

  expect(
    length(outputs) == 0,
    sprintf("%s produced %s.", lab, paste(outputs, collapse = ", "))
  )
  invisible(NULL)
}
