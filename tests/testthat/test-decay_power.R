context("Decay power")

# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

testthat::skip_on_cran()



default_tester <- function(decay_value = 0.5) {

  results <- accessibility::decay_power(decay_value =  decay_value)
  return(results)
}


# errors and warnings -----------------------------------------------------


test_that("adequately raises errors", {

  # incorrect input
  expect_error( default_tester(decay_value = -1) )
  expect_error( default_tester(decay_value = 'banana') )
  expect_error( default_tester(decay_value = NULL) )

})



# adequate behavior ------------------------------------------------------


test_that("output is correct", {

f_test  <- default_tester(decay_value = 0.5)

  expect_is(f_test(20), 'numeric')
  expect_equal(f_test(0.5), 1)
  expect_equal( round(f_test(5), digits = 4), 0.4472)

})
