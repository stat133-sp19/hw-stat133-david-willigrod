context("Check checking function arguments")

test_that("check_prob fails with invalid args",{
  
  expect_true(check_prob(.5))
  expect_error(check_prob(2))
  expect_error(check_prob(-.5),
               "prob has to be a numeric value between 0 and 1")
})


test_that("check_trials fails with invalid args", {
  
  expect_true(check_trials(100))
  expect_error(check_trials(-2))
  expect_error(check_trials(10.5),
               "invalid number of trials")
})


test_that("check_success fails with invalid args", {
  
  expect_true(check_success(3, 5))
  expect_error(check_success(-1, 5), 
               "Success values must be positive integers")
  expect_error(check_success(6, 5),
               "number of success must be less than or equal to trials")
})