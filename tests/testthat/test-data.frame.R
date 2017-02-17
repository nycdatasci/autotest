library(autotest)
tryCatch(source('error.R'),
         error = function(e) source('tests/testthat/error.R')
)

Pet = data.frame(species = c('dog', 'cat', 'dog'),weight = c(20,1,40))
test_that('test Pet', {
  answer = data.frame(species=c('dog', 'cat', 'dog'), weight=c(20,1,40))
  expect_equal(Pet, answer)
})

error_code = c("Pet = data.frame(species = c('dog', 'cat', 'dog'),weight = c(20,1,40))
test_that('test Pet', {
  answer = data.frame(species=c('dog', 'cat', 'dog'), weight=c(20,10,40))
  expect_equal(Pet, answer)
})")

error_test(error_code)
