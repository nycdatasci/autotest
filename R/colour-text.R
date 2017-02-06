# fix the check NOTE:Namespace in Imports field not imported from: 'crayon'
#' @importFrom crayon green yellow red
autotest_colours <- list(
  success = green,
  skip = yellow,
  warning = yellow,
  failure = red,
  error = red
)

colourise <- function(text, as = c("success", "skip", "warning", "failure", "error")) {
  colour_config <- getOption("autotest.use_colours", TRUE)
  if (!isTRUE(colour_config)) {
    return(text)
  }

  as <- match.arg(as)
  autotest_colours[[as]](text)
}

