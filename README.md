# autotest

This package is edit from the [testthat]() package. The main goal is makeing the error message human readable.

The usage of this packages is also the same with the `testthat` package. 

- Document: [autotest-docs.readthedocs.io](autotest-docs.readthedocs.io/en/latest/R/index.html)

- Slides: [Autotest with R](https://github.com/casunlight/autotest/blob/master/inst/slides/index.Rmd)


## Design Logical

Actually there is **no new design** at all, just edit.

### Error Message Hooks

#### Design
The `autotest:::ErrorHandlerClass` is a class for register error messages. It offers 6 APIs. 

Four for error messages: `registerPreMsg`, `registerPostMsg`, `addPreMsg`, `addPostMsg`;

And two for turning on/off detecting variables/expressions: `turnOnTraceback`, `turnOffTraceback`.

The hidden variable `autotest:::ErrorHandler` is a instance of `ErrorHandlerClass`.

#### How it works?

Call Flow:

1. `test_that` -> `test_code` -> `eval(...)` -> `as.expectation.error`.

This occurs when running code trigger meet some errors. For exmaple, calling `expect_equal(f(1), 2)` without defining the function `f`.

The `format.expectation_error` functoin calls `autotest:::ErrorHandler$msg`, which deal with the error message hooks.

2. `test_that` -> `test_code` -> `expect_XXX` -> `expect`.

This occurs when meet failure expectattions. For exmaple, `expect_equal(1, 2)`.

```r
  if (expectation_broken(exp)) {
      exp$message <- ErrorHandler$msg(exp$message)  # add hooked error message
      stop(exp)
  }
```


BTW, Originally in the `testthat` package, `handle_expectation` does not stop when it meet expect error. Here are the code in function `handle_expectation` to stop it:

```r
    if (expectation_broken(e)){
      return()
    }
```


### Customized Error Message

This should be defined in each `expect_XXX` function.

Let us take the `expect_equal` function as an example. Here is the call flow: `expect_equal` -> `compare`. 

The `compare` function is generic function, which contains `compare.integer`, `compare.data.frame`, `compare.matrix` and other methods. That is how `expect_equal` supports multiple data types and show different kinds of error messages.

If you want to support a new object, let is say, a `ggplot` object. Implement a `compare.ggplot` method, set up the test logical and customize error messages. For example, check the `compare.data.frame` method you will see that it tests the class, dimension and columns step by step.
