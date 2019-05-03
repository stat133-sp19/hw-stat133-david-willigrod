context("Check main functions")

test_that("check bin_choose has proper args and returns correct result", {
  
  expect_error(bin_choose(5, 6), 
               "k cannot be greater than n")
  expect_equal(bin_choose(5, 2), 
               10)
  expect_equal(bin_choose(10, 1:3), 
               c(10, 45, 120))
})


test_that("check bin_probability has proper args and returns corrct results", {
  
  expect_error(bin_probability(6, 5, .5))
  expect_equal(bin_probability(2, 5, .5), 
               0.3125)
  expect_equal(bin_probability(0:2, 5, .5), 
               c(.03125, 0.15625, 0.31250))
})


test_that("check bin_distribution has proper args and returns correct results", {
  
  expect_error(bin_distribution(5, 2))
  expect_equal(head(bin_distribution(5, .5)$success),
               0:5)
  expect_equal(head(bin_distribution(5, .5)$probability), 
               c(0.03125, 0.15625, 0.31250, 0.31250, 0.15625, 0.03125))
  
})


test_that("check bin_cumulative has proper args and returns correct results", {
  
  expect_error(bin_cumulative(5, 2))
  expect_equal(head(bin_cumulative(5, .5)$success),
               0:5)
  expect_equal(head(bin_cumulative(5, .5)$cumulative),
               c(0.03125, 0.18750, 0.50000, 0.81250, 0.96875, 1.00000))
})
  