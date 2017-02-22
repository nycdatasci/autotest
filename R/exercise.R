#' Parse a Rmd file to an exercise
#'
#' @param input Input file (Rmd)
#' @return A list, contains all the elements of exercise
#' @examples
#' path = system.file('rmarkdown/templates/skeleton/skeleton.Rmd', package = 'autotest')
#' parse_exercise(path)
#' @export
parse_exercise <- function(input=''){
  if (!file.exists(input)){
    stop(sprintf('Can not open file: %s', input))
  }
  res = list()
  ## yaml header ------------------------------------------
  output <- rmarkdown::yaml_front_matter(input)
  res$title <- output$title
  res$user <- output$author
  res$difficult <- as.integer(output$output$html_document$difficult)
  res$timeout <- as.integer(output$output$html_document$timeout)
  res$key <- tolower(trimws(strsplit(output$output$html_document$key, ',')[[1]]))

  ## code ------------------------------------------
  txt = readLines(input)
  local({
    library(knitr)
    knitr:::knit_code$restore()
    on.exit(knitr:::knit_code$restore())
    knitr:::set_pattern('md')
    knitr:::opts_knit$set(out.format='markdown')
    invisible(knitr:::split_file(txt))

    chunk_code <- function(label){
      res = as.character(knitr:::knit_code$get(label))
      paste(res, collapse = '\n')
    }
    res$pre_code <<- chunk_code('pre_code')
    res$set_code <<- chunk_code('set_code')
    res$ans_code <<- chunk_code('ans_code')
    res$test_code <<- chunk_code('test_code')
  })

  ## body & hint ------------------------------------------
  get_keyword <- function(lines, keyword = NULL){
    stopifnot(is.character(keyword))
    index = grep('^##', lines)
    start_idx = grep(paste0('## +', keyword), lines, ignore.case = TRUE, perl= T)
    end_inx = index[index > start_idx][1]
    stopifnot(end_inx-1 >= start_idx+1)
    trimws(paste(lines[(start_idx+1):(end_inx-1)], collapse = '\n'))
  }
  res$body <- get_keyword(txt, 'body')
  res$hint <- get_keyword(txt, 'hint')
  return(res)
}
