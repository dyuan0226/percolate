library(testthat)


#Unit tests for board()

test_that("board() retains structure on valid mat inputs", {
  test <- generate_board_mat() 
  board_test <- board(mat=test) #attributes should be n=5, p=0.24, empirically
  expect_equivalent(test, unclass(board_test))
  expect_equivalent(attr(board_test, "n"), 5)
  expect_equivalent(attr(board_test, "p"), 0.24)
})

test_that("board() fails with incorrect mat structure", {
  bad_mat <- matrix(sample(0:2, size=30, replace=TRUE), nrow=6)
  expect_error(board(mat=bad_mat))
})

test_that("board() rejects incorrect n and p values", {
  expect_error(board(n=0.5))
  expect_error(board(n=-3))
  expect_error(board(p=1.5))
  expect_error(board(p=c(TRUE, FALSE)))
  expect_error(board(n="asdf"))
  expect_error(board(p=-0.1))
  expect_error(board(n=1:5))
})
