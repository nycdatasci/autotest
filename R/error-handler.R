library(R6)
ErrorHandlerClass <- R6::R6Class("ErrorMessage",
   public = list(
     trace = TRUE,
     pre = NULL,
     post = NULL,
     testing = NULL,
     initialize = function() {
       self$trace <- TRUE
       self$pre <- NULL
       self$post <- NULL
       self$testing = NULL
     },
     setTesting = function(...){
       msg = sprintf(...)
       stopifnot(is.character(msg))
       self$testing = msg
       invisible(msg)
     },
     getTesting = function(){
       res = self$testing
       self$testing <- NULL
       ifelse(all(is.null(res)), '', res)
     },
     # pre error message
     setPre = function(x){
       self$pre <- x
       invisible(x)
     },
     registerPreMsg = function(...){
       msg = sprintf(...)
       stopifnot(is.character(msg))
       self$pre <- msg
       invisible(msg)
     },
     addPreMsg = function(...){
       msg = sprintf(...)
       stopifnot(is.character(msg))
       if ( length(self$pre) == 0 ){
         self$pre <- msg
       }else{
         self$pre <- c(self$pre, msg)
       }
       invisible(msg)
     },

     # post error message
     setPost = function(x){
       self$post <- x
       invisible(x)
     },
     registerPostMsg = function(...){
       msg = sprintf(...)
       stopifnot(is.character(msg))
       self$post <<- msg
       invisible(msg)
     },
     addPostMsg = function(...){
       msg = sprintf(...)
       stopifnot(is.character(msg))
       if ( length(self$post) == 0 ){
         self$post <- msg
       }else{
         self$post <- c(self$post, msg)
       }
       invisible(msg)
     },

     # get message
     getPreMsg = function(){
       msg = self$pre
       self$pre <- NULL
       return(msg)
     },
     getPostMsg = function(){
       msg = self$post
       self$post <- NULL
       return(msg)
     },

     # trace or not
     offTrace = function(){
       self$trace <- FALSE
     },
     onTrace = function(){
       self$trace <- TRUE
     },

     # message
     msg = function(error_message){
       stopifnot(is.character(error_message))
       msg_vector = c('AutoTestCaseError:',
                      ifelse(self$trace, self$getTesting(), ''),
                      self$getPreMsg(), error_message, self$getPostMsg())
       msg_vector = Filter(function(x) x != '' && !is.null(x), msg_vector)
       res = paste(msg_vector, collapse = "\n")
       return(res)
     }
   )
)

ErrorHandler <- ErrorHandlerClass$new()

getPreMsg <- function(){
  ErrorHandler$getPreMsg()
}

getPostMsg <- function(){
  ErrorHandler$getPostMsg()
}

getTesting <- function(){
  ErrorHandler$getTesting()
}


#' Self-defined error messages
#'
#' Once there are unexpectations, add error messages in the output.
#'
#' @details The arguments are exactly the same with the \code{\link{sprintf}} function.
#'
#' \code{registerPreMsg} and \code{registerPostMsg} set up the customized messages
#' befor and after the error messages. If you set up multiple times, the last one will
#' overwritte all the previous settings.
#'
#' \code{registerPreMsg} and \code{registerPostMsg} are doing append things instead of
#' overwritte.
#' @examples
#' registerPreMsg('testing f(%d)', 123) # just like sprintf('testing f(%d)', 123)
#'
#' f = function(x) ifelse(x<5, x+1, x)
#' # customize error messages
#' \dontrun{
#'  test_that('', {
#'    for (i in 1:5){
#'      registerPreMsg('In testing f(%d)', i)  # do not use `addPreMsg` here!
#'      expect_equal(f(i), i + 1)
#'    }
#   })
#' }
#' @name error-handler
NULL

#' @export
#' @rdname error-handler
registerPreMsg <- function(...){
  ErrorHandler$registerPreMsg(...)
}

#' @export
#' @rdname error-handler
registerPostMsg <- function(...){
  ErrorHandler$registerPostMsg(...)
}

#' @export
#' @rdname error-handler
addPreMsg <- function(...){
  ErrorHandler$addPreMsg(...)
}

#' @export
#' @rdname error-handler
addPostMsg <- function(...){
  ErrorHandler$addPostMsg(...)
}



#' Trun On/Off Tracking objects
#'
#' Whether automatically detect the variables/expressions in \link{expect_equal}, \link{expect_true}, \link{expect_false} and etc.
#' Default it is TRUE.
#'
#' @examples
#' x = 1
#' \dontrun{
#' turnOnTraceback()
#' test_that('', {
#'   expect_equal(x, 2)
#' })
#' turnOffTraceback()   # turn off detecting, see the error message
#' test_that('', {
#'   expect_equal(x, 2)
#' })
#' }
#' @name trace
NULL


#' @export
#' @rdname trace
turnOnTraceback <- function(){
  ErrorHandler$onTrace()
}


#' @export
#' @rdname trace
turnOffTraceback <- function(){
  ErrorHandler$offTrace()
}


