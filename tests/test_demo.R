setwd("..")
invisible(source("Understanding_PLSPM_Formulation.R", chdir = TRUE,print.eval = F))
library(testthat)

#### test sd.p(x)
test_that("population sd", {
  x <- c(2,4,4,4,5,5,7,9)
  expect_equal(sd.p(x),2)
})

#### test stand_vecotr(x)
# vectors should have mean=0 and sd=1
test_that("standardize vector calculated correctly", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(stand_vector(x), c(-1.414, -0.707, 0, 0.707, 1.414), tolerance = 0.001)
})

test_that("standardize vector mean is calculated correctly", {
  x <- c(2,4,4,2,5,5,-4,30,3,2,2,6,8,3,-2,10)
  expect_equal(mean(stand_vector(x)),0, tolerance = 0.001)
})

test_that("standardize vector s.d is calculated correctly", {
  x <- c(2,4,4,2,5,5,-4,30,3,2,2,6,8,3,-2,10)
  expect_equal(sd(stand_vector(x)), 1, tolerance = 0.001)
})


test_that("cov.p calculates the correct covariance 
          for different input scenarios", {
 #https://stackoverflow.com/questions/30695648/how-to-redefine-cov-to-calculate-population-covariance-matrix
  
  # Test case 1: Basic test with two vectors
  x1 <- c(1, 2, 3, 4, 5)
  y1 <- c(2, 4, 6, 8, 10)
  result1 <- cov.p(x1, y1)
  expected1 <- cov(x1, y1) * 4 / 5
  expect_equal(result1, expected1)
  
  # Test case 3: Test with a vector and a matrix
  x3 <- c(1, 2, 3, 4, 5)
  y3 <- matrix(c(2, 4, 6, 8, 10, 3, 6, 9, 12, 15), ncol = 2)
  result3 <- cov.p(x3, y3)
  expected3 <- cov(x3, y3) * 4 / 5
  expect_equal(result3, expected3)
  
  # Test case 4: Test with two matrices
  x4 <- matrix(c(1, 2, 3, 4, 5), ncol = 1)
  y4 <- matrix(c(2, 4, 6, 8, 10), ncol = 1)
  result4 <- cov.p(x4, y4)
  expected4 <- cov(x4, y4) * 4 / 5
  expect_equal(result4, expected4)
})

