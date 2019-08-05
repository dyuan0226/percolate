library(testthat)


#Unit tests for percolate.board()

test_that("board of 1's percolates", {
  my_board <- board(matrix(1, nrow=10, ncol=10))
  res <- percolate(my_board)
  expect_true("list" == class(res))
  result_board <- res$`result_board`
  result <- res$result
  expect_true(result)
  expect_true(all.equal(unclass(result_board), matrix(2, nrow=10, ncol=10), check.attributes=FALSE))
})

test_that("board of 0's does not percolate", {
  my_board <- board(matrix(0, nrow=10, ncol=10))
  res <- percolate(my_board)
  expect_true("list" == class(res))
  result_board <- res$`result_board`
  result <- res$result
  expect_false(result)
  expect_true(all.equal(unclass(result_board), matrix(0, nrow=10, ncol=10), check.attributes=FALSE))
})

test_that("board with top row blocked will not percolate", {
  my_board <- board(matrix(1, nrow=10, ncol=10))
  my_board[1,] <- 0
  res <- percolate(my_board)
  expect_true("list" == class(res))
  result_board <- res$`result_board`
  result <- res$result
  expect_false(result)
  
  result_mat <- matrix(1, nrow=10, ncol=10)
  result_mat[1,] <- 0
  expect_true(all.equal(unclass(result_board), result_mat, check.attributes=FALSE))
})

test_that("board with bottom row blocked will not percolate", {
  my_board <- board(matrix(1, nrow=10, ncol=10))
  my_board[attr(my_board, "n"),] <- 0
  res <- percolate(my_board)
  expect_true("list" == class(res))
  result_board <- res$`result_board`
  result <- res$result
  expect_false(result)
  
  result_mat <- matrix(2, nrow=10, ncol=10)
  result_mat[10,] <- 0
  expect_true(all.equal(unclass(result_board), result_mat, check.attributes=FALSE))
})

test_that("percolate.board() works with all the test cases",{
  load(url("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolate_test.Rdata"))
  
  your_result_list <- lapply(board_list, percolate)
  
  bool_vec <- sapply(1:length(result_list), function(x){
    your_board <- your_result_list[[x]]$result_board
    result_board <- result_list[[x]]$result_board
    
    identical(your_board, result_board) * 
      (your_result_list[[x]]$result == result_list[[x]]$result)
  })
  
  expect_true(all(bool_vec))
})