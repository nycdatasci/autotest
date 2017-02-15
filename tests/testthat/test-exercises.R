detachAllPackages <- function() {
  basic.packages <- c("package:autotest", "package:console", "package:plumber",
                      "package:stats", "package:graphics", "package:grDevices",
                      "package:utils", "package:datasets", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list) == 0) return(invisible())
  for (package in package.list) detach(package, character.only=TRUE)
}


tryCatch(
  {
    library(jsonlite)
    rex <- fromJSON(readLines('rex.json'))
  },
  error = function(e) rex <<- fromJSON(readLines('tests/testthat/rex.json'))
)

edit_code <- function(x){
  x = gsub('testthat', 'autotest', x)
  gsub('expect_identical', 'expect_equal', x)
}

run_code <- function(code){
  detachAllPackages()
  tryCatch({
      eval(parse(text=code))
    },
    expectation = function(e){
      stop(e$message)
    },
    error = function(e){
      stop(e$message)
    }
  )
}

new_code <- readLines('tests/testthat/new_test.R')
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

## 55, 68,
for (i in 1:nrow(rex)){
  row_code = rex[i, ]
  # new_test = fetch_new_test(row_code$id)
  # if (!is.null(new_test)){
  #   row_code$test_code = new_test
  # }
  code = paste(row_code$pre_code, row_code$ans_code, row_code$test_code, sep='\n')
  code = edit_code(code)
  cat(i, ' done, id: ', row_code$id, '\n')
  run_code(code)
}

