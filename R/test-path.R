#' Locate file in testing directory.
#'
#' This function is designed to work both interatively and during tests,
#' locating files in the `tests/autotest` directory
#'
#' @param ... Character vectors giving path component.
#' @return A character vector giving the path.
#' @export
test_path <- function(...) {
  if (in_testing_dir(".")) {
    path <- file.path(...)
    if (length(path) == 0) {
      path <- "."
    }
  } else {
    base <- "tests/autotest"
    if (!file.exists(base)) {
      stop("Can't find `tests/autotest` in current directory.",
        call. = FALSE)
    }
    path <- file.path(base, ...)
  }

  path
}

in_testing_dir <- function(path) {
  path <- normalizePath(path)

  if (basename(path) != "autotest")
    return(FALSE)

  parent <- dirname(path)
  if (grepl("-tests$", parent)) {
    # Probably called from tools::testInstalledPackage
    TRUE
  } else {
    basename(parent) %in% c("tests", "tests_x64", "tests_i386")
  }
}
