library(testthat)
library(tidyverse)







test_that("generate_board_mat() produces 5x5 matrix that is 0.25 blocked", {
  test <- generate_board_mat()
  expect_equal(sum(test), 6)
  expect_true(length(test) == 25)
})

test_that("generate_board_mat(n=4) produces 4x4 matrix that is 0.25 blocked", {
  test <- generate_board_mat(n=4)
  expect_equal(sum(test), 4)
  expect_true(length(test) == 16)
})

test_that("generate_board_mat(p=0) produces 5x5 matrix that is all 0's", {
  test <- generate_board_mat(p=0)
  expect_equal(sum(test), 0)
  expect_true(length(test) == 25)
})

test_that("generate_board_mat(p=1) produces 5x5 matrix that is all 1's", {
  test <- generate_board_mat(p=1)
  expect_equal(sum(test), 25)
  expect_true(length(test) == 25)
})

test_that("generate_board_mat() produces 5x5 matrix that is 0.25 blocked", {
  test <- generate_board_mat()
  expect_equal(sum(test), 6)
  expect_true(length(generate_board_mat()) == 25)
})

test_that("testing for valid inputs of generate_board_mat()", {
  expect_error(generate_board_mat(n=c(1, 2)))
  expect_error(generate_board_mat(n="asdf"))
  expect_error(generate_board_mat(n=5.4))
  expect_error(generate_board_mat(n=-5))
})


