# check error submissions
library(autotest)

edit_code <- function(x){
  x = gsub('testthat', 'autotest', x)
  gsub('expect_identical', 'expect_equal', x)
}

run_test <- function(df, start=1){
  stopifnot(start < nrow(df))
  new_code <- readLines('tests/testthat/new_test.txt')
  split_index = grep('#[0-9a-z]{24}', new_code)
  fetch_new_test = function(id){
    target_index = grep(paste('#', id, sep=''), new_code)
    if (length(target_index) > 1){
      warning(sprintf('There are two version of test code. id: %s', id))
      target_index = target_index[1]
    }else if(length(target_index) == 0){
      return(NULL)
    }
    end_index = split_index[split_index > target_index[1]]
    end_index = ifelse(length(end_index) >= 1, end_index[1], length(new_code))
    paste(new_code[target_index:end_index], collapse = '\n')
  }

  for (i in start:nrow(df)) {
    row_code = df[i, ]
    message(i, ' row, running ex id: ', row_code$id, '\n')
    cat('----------------------------------\n')

    ## syntax
    tryCatch({
      is_syntax_error <<- console::check_syntax(row_code$code)
    }, error = function(e) is_syntax_error <<- data.frame())
    if (is.data.frame(is_syntax_error)) {
      message('syntax error')
      next
    }

    ## test code
    new_test = fetch_new_test(row_code$id)
    if (!is.null(new_test)){
      row_code$test_code = new_test
    }
    code = paste(row_code$pre_code, row_code$code, row_code$test_code, sep='\n')
    code = edit_code(code)

    ## run code
    res = console::run_console(code, env = new.env())
    res = Filter(function(x) x$type == 'error', res)
    for (i in res){
      message(i$string, '\n')
    }
    edit_test = readline('edit (y/n)?')
    if (edit_test == 'y'){
      message('#', row_code$id,'------------------------\n', sep = '')
      cat(edit_code(row_code$test_code),'\n')
      message('#error submission.......................\n')
      cat(row_code$code, '\n')
      next_one = readline('continue (y/n)?')
    }else{
      next_one = 'y'
    }
    if (startsWith(next_one, 'y')) {
      next()
    }else{
      break()
    }
  }
}

run_main <- function(){
  withCallingHandlers(
    rex <<- jsonlite::fromJSON(readLines('tests/testthat/rsubmission.json')),
    error = function(e) rex <<- jsonlite::fromJSON(readLines('rsubmission.json')),
    warning = function(e) invisible()
  )
  rex <<- unique(rex)
  # run_test(rex, 2088)
  # library(dplyr)
  # sample_res <- rex %>% group_by(id) %>% do(sample_n(., min(10, nrow(.))))
  # run_test(sample_res, 460)
}

# run_main()
