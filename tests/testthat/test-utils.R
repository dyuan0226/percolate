library(testthat)


#Unit tests for generate_board_mat()

test_that("generate_board_mat() produces 5x5 matrix that is 0.25 blocked", {
  test <- generate_board_mat()
  expect_equal(sum(test), 19)
  expect_true(length(test) == 25)
})

test_that("generate_board_mat(n=4) produces 4x4 matrix that is 0.25 blocked", {
  test <- generate_board_mat(n=4)
  expect_equal(sum(test), 12)
  expect_true(length(test) == 16)
})

test_that("generate_board_mat(p=0) produces 5x5 matrix that is all 0's", {
  test <- generate_board_mat(p=0)
  expect_equal(sum(test), 25)
  expect_true(length(test) == 25)
})

test_that("generate_board_mat(p=1) produces 5x5 matrix that is all 1's", {
  test <- generate_board_mat(p=1)
  expect_equal(sum(test), 0)
  expect_true(length(test) == 25)
})

test_that("generate_board_mat() produces 5x5 matrix that is 0.25 blocked", {
  test <- generate_board_mat()
  expect_equal(sum(test), 19)
  expect_true(length(generate_board_mat()) == 25)
})

test_that("testing for valid inputs of generate_board_mat()", {
  expect_error(generate_board_mat(n=c(1, 2)))
  expect_error(generate_board_mat(n="asdf"))
  expect_error(generate_board_mat(n=5.4))
  expect_error(generate_board_mat(n=-5))
})


# Unit tests for is_valid()

test_that("valid board must be a matrix", {
  fail_board <- sample(0:2, size=25, replace=TRUE)
  expect_error(is_valid(fail_board))
})

test_that("valid board must contain only 0's, 1's, 2's", {
  fail_board <- matrix(sample(0:3, size=100, replace=TRUE), nrow=10)
  expect_error(is_valid(fail_board))
})

test_that("valid board must be square", {
  fail_board <- matrix(sample(0:2, size=30, replace=TRUE), nrow=6)
  expect_error(is_valid(fail_board))
})