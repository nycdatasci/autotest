# autotest

This package is edit from the [testthat]() package. The main goal is makeing the error message human readable.

The usage of this packages is also the same with the `testthat` package. 

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

1. `test_that` -> `test_code` -> `eval(...)` -> `format.expectation_error`.

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


## TODO

### 1. Hack the `test_that` function.
Hack it to stop testing once meet an error. 

```r
f <- function(x) ifelse(x > 5, x + 1, x)
test_that('',{
  for (i in 1:10){
    expect_equal(f(i), i + 1)  # The goal is stop testing when i == 5 
  }
})
```

### 2. Self-defined messages APIs

Example:

```r
f <- function(x) ifelse(x > 5, x + 1, x)
test_that('',{
  for (i in 1:10){
    registerPreMsg('In testing f(%d)', i) # add msg for better understanding
    expect_equal(f(i), i + 1)
  }
})
# AutoTestCaseError:
# Testing variable/expression:  f(i)
# In testing f(1)
# Your answer is 1, which is not equal to the correct answer 2
```

### 3. Implement testing functions.

- `expect_equal`: done
- `expect_false`, `expect_true`, `expect_na`: done
- `expect_is`, `expect_length` : done
- `expect_match`, `expect_error`, `expect_output`: done


It is better to keep a little bit part of them, keep the most useful functions.

### 4. Mannual

It is better to have a website with 4(may have more in the future) pages:
(1) Quick start.

(2) Introduce testing functions with examples. The most important one is `expect_equal`, it support a lot of data types (`integer`, `character`, `logical`, `double`, `factor`, `list`, `matrix`, `data.frame`). Furthermore, it is easy to extend.

(3) Error messages APIs. Register messages to make the error easy understanding.

(4) Develop guide. 

### 5. Testing exercieses and push online
