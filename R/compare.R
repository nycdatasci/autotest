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
  if (any(is.na(x))){
    msg = 'Your answer conatins missing values NA, please check again.'
    return(comparison(FALSE, msg))
  }
  if ( is.numeric(x) && is.numeric(y) ){
    UseMethod("compare", y)
  }else if (x_class != y_class &&  !inherits(x, y_class)){
    msg = sprintf('We expect your answer returns type "%s", but it returns "%s" instead.', y_class, x_class)
    if (all(is.null(x) || is.na(x))){
      msg = paste(msg,
                  'Do you forget to return something in your function definition?',
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
compare.integer <- function(x, y, ...){
  # test length
  x_length = length(x); y_length = length(y)
  length_res = compare_length(x, y)
  if (length_res$equal != TRUE) return(length_res)

  # test values
  if (all(x - y < 1e-15)){
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
  if (x_length == 1){
    if (x == y) return(comparison())
    msg = sprintf('Your answer is "%s", which is not equal to the correct answer "%s"', x, y)
    return(comparison(FALSE, msg))
  }else{
    if (all(x == y)) return(comparison())
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
  compare_result = abs(x - y) <= tolerance
  if (all(compare_result)) return(comparison())
  if (x_length == 1){
    msg = sprintf('Your answer is %s, which is not equal to the correct answer %s', x, y)
    return(comparison(FALSE, msg))
  }else{
    index = which(!compare_result)[1]
    msg = sprintf('The %sth element of your vector is %s, which is not equal to the correct answer %s', index, x[index], y[index])
    return(comparison(FALSE, msg))
  }
}

#' @export
compare.factor <- function(x, y, ...){
  # test length
  x_length = length(x); y_length = length(y)
  length_res = compare_length(x, y)
  if (length_res$equal != TRUE) return(length_res)

  # test levels
  x_levels = levels(x); y_levels = levels(y)
  if ( all(x_levels != y_levels) ){
    msg = 'The levels of your factor is: %s.\nWhile the levles of the correct answer is: %s'
    return(comparison(FALSE, sprintf(msg, x_levels, y_levels)))
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
  if ( test.names && all(names(x) != names(y)) ){
    msg=sprintf('The names of your list is %s, which is not euqal to the correct answer %s', names(x), names(y))
    return(comparison(FALSE, msg))
  }

  # test values
  for (i in seq_along(x)){
    if ( all.equal(x[[i]], y[[i]], ...) ) {
      msg = sprintf('The %sth element of your list is %s, which is not equal to the correct answer %s',
                    index, x[[i]], y[[i]])
      return(comparison(FALSE, msg))
    }
  }
  return(comparison())
}

#' @export
compare.matrix <- function(x, y, ..., tolerance = 1e-5){
  # test dimension
  x_dim=dim(x); y_dim = dim(y)
  if (!all(x_dim == y_dim)){
    msg = sprintf('The dimension of your matrix is (%s), which is not equal to the dimension of correct answer: (%s).',
                  paste0(x_dim, collapse = ','),
                  paste0(y_dim, collapse = ',')
    )
    return(comparison(FALSE, msg))
  }

  # test values
  if ( (class(x[1, 1]) == 'numeric' && class(y[1, 1]) == 'numeric') ){
    compare_res = abs(x - y) <= tolerance
  }else{
    compare_res = x == y
  }
  if (!all(compare_res)){
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
      msg = sprintf('The columns names of your data.frame are (%s). It does not contain the following columns: (%s)',
                    paste(colnames(x), collapse = ','),
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
    # test class of column
    x_class = class(x[, col]); y_class = class(y[, col])
    is_col_numeric = is.numeric(x) && is.numeric(y)
    if (x_class != y_class & !is_col_numeric){
      msg = sprintf('The data type of the column "%s" in your data.frame is %s, which should be %s',
                    col, x_class, y_class)
      return(comparison(FALSE, msg))
    }

    # test value of column
    if (y_class == 'numeric'){
      col_val_diff = abs(x[[col]] - y[[col]]) <= tolerance
    }else{
      col_val_diff = x[[col]] == y[[col]]
    }
    if (!all(col_val_diff)){
      index = which(!col_val_diff)[1]
      msg = sprintf('The %ith row, "%s" column of your data.frame is %s, which is not equal to the correct answer %s',
                    index, col, x[index, col], y[index, col])
      return(comparison(FALSE, msg))
    }
  }

  return(comparison())
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

