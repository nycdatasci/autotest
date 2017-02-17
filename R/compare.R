mark_type <- function(cls){
  stopifnot(is.character(cls))
  if (length(cls) == 1) return(cls)
  support_types = c('tbl_df', 'data.frame', 'list', 'matrix', 'factor',
                    'logical', 'integer', 'numeric', 'character')
  for (i in support_types){
    if (i %in% cls) return(i)
  }
  return(cls[1])
}

#' Provide human-readable comparison of two objects
#'
#' `compare` is similar to [base::all.equal()], but shows
#' you examples of where the failures occured.
#' Support data types: integer, numeric, logical, character, factor, list, matrix, data.frame
#'
#' @export
#' @param x,y Objects to compare
#' @param tolerance Numerical tolerance: any differences smaller than this
#'   value will be ignored. Default, 1e-5.
#' @param test.names If x and y are lists, whether to test the names of elements. Default, TRUE.
#' @param test.rowname If x and y are data frames, whether to test the row names. Default, FALSE.
#' @param test.colname If x and y are data frames, whether to test the column names. Default, TRUE.
#' @param ... Additional arguments used to control specifics of comparison
#' @description The parameters `test.rowname` and `test.colname` only works for data frames. While
#' the `test.names` parameter only works for list.
compare <- function(x, y, ...) {
  x_class = class(x); y_class = class(y)
  if (identical(x, y)) return(comparison())
  try(
    if (anyNA(x)){
      msg = 'Your answer conatins missing values NA, please check again.'
      return(comparison(FALSE, msg))
    },
    silent=TRUE
  )
  if ( is.numeric(x) && is.numeric(y) ){
    UseMethod("compare", y)
  }else if (x_class != y_class &&  !inherits(x, y_class)){
    msg = sprintf('We expect your answer returns type "%s", but it returns "%s" instead.', mark_type(y_class), mark_type(x_class))
    if (all(is.null(x))){
      msg = paste(msg,
                  '\nDo you forget to return something in your function definition?',
                  sep = '\n'
                  )
    }
    return(comparison(FALSE, msg))
  }
  UseMethod("compare", x)
}

comparison <- function(equal = TRUE, message = "Equal") {
  stopifnot(is.logical(equal), length(equal) == 1)
  stopifnot(is.character(message))

  structure(
    list(
      equal = equal,
      message = paste(message, collapse = "\n")
    ),
    class = "comparison"
  )
}
difference <- function(..., fmt = "%s") {
  comparison(FALSE, sprintf(fmt, ...))
}
no_difference <- function() {
  comparison()
}

#' @export
print.comparison <- function(x, ...) {
  if (x$equal) {
    cat("Equal\n")
    return()
  }

  cat(x$message)
}

print_out <- function(x, ...) {
  lines <- capture_output_lines(x, ..., print = TRUE)
  paste0(lines, collapse = "\n")
}

#' @export
compare_length <- function(x, y){
  x_length = length(x); y_length = length(y)
  if (x_length != y_length){
    msg = sprintf('The length of your answer is %s, which should be %s in the correct answer', x_length, y_length)
    return(comparison(FALSE, msg))
  }else{
    return(list(equal=TRUE))
  }
}

## TODO
# 1. time object?
# 2. ggplot object?
# 3. shiny object?

# Compare methods ---------------------------------------------------------------
#' @export
#' @rdname compare
compare.default <- function(x, y, ..., max_diffs = 9){
  same <- all.equal(x, y, ...)
  if (length(same) > max_diffs) {
    same <- c(same[1:max_diffs], "...")
  }

  comparison(identical(same, TRUE), as.character(same))
}

#' @export
compare.integer <- function(x, y, ..., tolerance=1e-15){
  # test length
  x_length = length(x); y_length = length(y)
  length_res = compare_length(x, y)
  if (length_res$equal != TRUE) return(length_res)

  # test values
  if (all(x - y < tolerance) || all(x == y)){
    return(comparison())
  }
  if (x_length == 1){
    msg = sprintf('Your answer is %s, which is not equal to the correct answer %s', x, y)
    return(comparison(FALSE, msg))
  }else{
    index = which(x != y)[1]
    msg = sprintf('The %sth element of your vector is %s, which is not equal to the correct answer %s', index, x[index], y[index])
    return(comparison(FALSE, msg))
  }
}

#' @export
compare.logical <- compare.integer

#' @export
compare.character <- function(x, y, ...){
  # test length
  x_length = length(x); y_length = length(y)
  length_res = compare_length(x, y)
  if (length_res$equal != TRUE) return(length_res)

  # test values
  if (identical(as.character(x), as.character(y))) return(comparison())
  if (x_length == 1){
    msg = sprintf('Your answer is "%s", which is not equal to the correct answer "%s"', x, y)
    return(comparison(FALSE, msg))
  }else{
    index = which(x != y)[1]
    msg = sprintf('The %sth element of your vector is "%s", which is not equal to the correct answer "%s"', index, x[index], y[index])
    return(comparison(FALSE, msg))
  }
}

#' @export
compare.numeric <- function(x, y, ..., tolerance = 1e-5){
  # test length
  x_length = length(x); y_length = length(y)
  length_res = compare_length(x, y)
  if (length_res$equal != TRUE) return(length_res)

  # test values
  compare_result = abs(x - y) <= tolerance | x == y
  if (all(compare_result)) return(comparison())
  if (x_length == 1){
    msg = sprintf('Your answer is %s, which is not equal to the correct answer %s', x, y)
  }else{
    index = which(!compare_result)[1]
    msg = sprintf('The %sth element of your vector is %s, which is not equal to the correct answer %s', index, x[index], y[index])
  }
  msg = paste(msg, '\nThe maximum tolerance is ', tolerance, sep='')
  return(comparison(FALSE, msg))
}

#' @export
compare.factor <- function(x, y, ...){
  # test length
  x_length = length(x); y_length = length(y)
  length_res = compare_length(x, y)
  if (length_res$equal != TRUE) return(length_res)
  # test sorted factor
  if (is.ordered(y) && !is.ordered(x)){
    msg = 'The answer is an ordered factor, your factor is not unordered.\nUse the `as.ordered` function to convert you answer to an ordered factor.'
    return(comparison(FALSE, msg))
  }
  # test levels
  x_levels = levels(x); y_levels = levels(y)
  if ( !identical(x_levels, y_levels) ){
    msg = sprintf('The levels of your factor is: [%s].\nWhile the levles of the correct answer is: [%s]',
                  paste0(x_levels, collapse = ', '),
                  paste0(y_levels, collapse = ', '))
    return(comparison(FALSE, msg))
  }

  # test values
  compare_res = as.character(x) == as.character(y)
  if ( !all(compare_res) ){
    index = which(!compare_res)[1]
    msg = sprintf('The %sth element of your factor is %s, which is not equal to the correct answer %s',
                  index, as.character(x[index]), as.character(y[index]))
    return(comparison(FALSE, msg))
  }
  return(comparison())

}

#' @export
compare.list <- function(x, y, ..., test.names = TRUE){
  # test length
  x_length = length(x); y_length = length(y)
  length_res = compare_length(x, y)
  if (length_res$equal != TRUE) return(length_res)

  # test names
  if ( test.names && any(names(x) != names(y)) ){
    msg=sprintf('The names of your list is [%s], which is not euqal to the correct answer [%s]',
                paste0(names(x), collapse = ', '),
                paste0(names(y), collapse = ', '))
    return(comparison(FALSE, msg))
  }

  # test values
  for (i in seq_along(x)){
    res = compare(x[[i]], y[[i]], ...)
    if (!res$equal) {
      msg = sprintf('The type of the %dth element in your list is `%s`.\nIn testing the %dth element:\n%s',
                    i, class(x[[i]]), i, res$message)
      return(comparison(FALSE, msg))
    }
  }
  return(comparison())
}

#' @export
compare.matrix <- function(x, y, ..., tolerance = 1e-5){
  # test dimension
  x_dim=dim(x); y_dim = dim(y)
  if (any(x_dim != y_dim)){
    msg = sprintf('The dimension of your matrix is (%s), which is not equal to the dimension of correct answer: (%s).',
                  paste0(x_dim, collapse = ','),
                  paste0(y_dim, collapse = ',')
    )
    return(comparison(FALSE, msg))
  }

  # test values
  if ( is.numeric(x) && is.numeric(y) ){
    compare_res = abs(x - y) <= tolerance | x == y
  }else if (class(x[1,1]) != class(y[1,1])){
    msg = sprintf('The type of the data in your matrix is `%s`, which should be `%s`',
                  class(x[1,1]), class(y[1,1])
                  )
    return(comparison(FALSE, msg))
  }else{
    compare_res = x == y
  }
  if (any(!compare_res)){
    index = which(!compare_res, arr.ind=T)[1, ]
    msg = 'The value in %sth row, %sth column of your matrix is %s, which is not equal to the correct answer %s'
    msg = sprintf(msg, index[1], index[2], x[index[1], index[2]], y[index[1], index[2]])
    return(comparison(FALSE, msg))
  }
  return(comparison())
}

#' @export
compare.data.frame <- function(x, y, ..., tolerance = 1e-5, test.rowname=FALSE, test.colname=TRUE){
  # test dimension
  x_dim=dim(x); y_dim = dim(y)
  if (!all(x_dim == y_dim)){
    msg = sprintf('The dimension of your data.frame is (%s), which is not equal to the dimension of correct answer: (%s).',
                  paste0(x_dim, collapse = ','),
                  paste0(y_dim, collapse = ',')
    )
    return(comparison(FALSE, msg))
  }

  # test column names
  if (test.colname){
    col_diff = setdiff(colnames(y), colnames(x))
    if ( length(col_diff) > 0){
      msg = sprintf('The column names of your data.frame are [%s].\nThe column names should be: [%s].\nYou columns do not contain: [%s].',
                    paste(colnames(x), collapse = ','),
                    paste(colnames(y), collapse = ','),
                    paste(col_diff, collapse = ',')
      )
      return(comparison(FALSE, msg))
    }
  }

  # test row names
  # if (test.rowname){
  #   col_diff = setdiff(rownames(y), rownames(x))
  # }

  # test values
  for (col in colnames(y)){
    res = compare(x[[col]], y[[col]], tolerance=tolerance, ...)
    if (!res$equal) {
      msg = sprintf('Testing the column `%s` in your data frame:\n%s',
                    col, res$message)
      return(comparison(FALSE, msg))
    }
  }

  return(comparison())
}

#' @export
compare.tbl_df <- function(x, y, ...){
  x = as.data.frame(x)
  y = as.data.frame(y)
  compare.data.frame(x, y, ...)
}

# Common helpers ---------------------------------------------------------------

same_length <- function(x, y) length(x) == length(y)
diff_length <- function(x, y) difference(fmt = "Lengths differ: %i vs %i", length(x), length(y))

same_type <- function(x, y) identical(typeof(x), typeof(y))
diff_type <- function(x, y) difference(fmt = "Types not compatible: %s vs %s", typeof(x), typeof(y))

same_class <- function(x, y) {
  if (!is.object(x) && !is.object(y))
    return(TRUE)
  identical(class(x), class(y))
}
diff_class <- function(x, y) {
  difference(fmt = "Classes differ: %s vs %s", klass(x), klass(y))
}

same_attr <- function(x, y) {
  is.null(attr.all.equal(x, y))
}
diff_attr <- function(x, y) {
  old <- options(useFancyQuotes = FALSE)
  on.exit(options(old), add = TRUE)

  out <- attr.all.equal(x, y)
  difference(out)
}

vector_equal <- function(x, y) {
  (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
}

vector_equal_tol <- function(x, y, tolerance = .Machine$double.eps ^ 0.5) {
  (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & abs(x - y) < tolerance)
}

