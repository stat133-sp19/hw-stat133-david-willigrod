context("Check summary measures")

test_that("private function aux_mean produces correct results", {

  expect_equal(aux_mean(6, .5),
               3)
  expect_equal(aux_mean(5, .5),
               2.5)
  expect_equal(aux_mean(20, 1),
                        20)
})


test_that("private function aux_variance produces correct results", {

  expect_equal(aux_variance(1, 1),
               0)
  expect_equal(aux_variance(2, .5),
               0.5)
  expect_equal(aux_variance(200, .75),
               37.5)
})


test_that("private function aux_mode produces correct results", {

  expect_equal(aux_mode(1, .5),
               c(1, 0))
  expect_equal(aux_mode(2, .75),
               2)
  expect_equal(aux_mode(1000, .25),
               250)
})


test_that("private function aux_skewness produces correct results", {

  expect_equal(aux_skewness(1000, .5),
               0)
  expect_equal(aux_skewness(1000, 0),
               Inf)
  expect_equal(aux_skewness(1000, 1),
               -Inf)
})


test_that("private function aux_kurtosis produces correct results", {

  expect_equal(aux_kurtosis(10, .5),
               -0.2)
  expect_equal(aux_kurtosis(10, 1),
               Inf)
  expect_equal(aux_kurtosis(1000, .5),
               -0.002)
})
