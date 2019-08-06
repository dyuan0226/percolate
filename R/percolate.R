library(assertthat)
library(gridExtra)


#' @export
percolate <- function(x, ...) UseMethod("percolate")

#' percolate.board()
#'
#' @param start_board specifies the board to test percolation of
#'
#' @return a list containing the board once water has been filled, and whether the board percolates
#'
#' @export
#' @examples
#' mat_example_list <- list(matrix(c(1,1,1,1,0,
#'                                   0,0,0,1,0,
#'                                   1,1,1,1,0,
#'                                   0,1,0,0,0,
#'                                   0,1,1,1,1), 5, 5),
#'                         matrix(c(1,1,1,1,0,
#'                                  0,0,0,1,0,
#'                                  0,1,1,1,0,
#'                                  0,1,0,0,0,
#'                                  0,1,1,1,1), 5, 5),
#'                         matrix(c(1,1,1,1,0,
#'                                  0,0,0,1,0,
#'                                  0,1,1,0,0,
#'                                  0,1,0,0,0,
#'                                  0,1,1,1,1), 5, 5))
#'
#' board_example_list <- lapply(mat_example_list, FUN=board)
#' results <- lapply(board_example_list, percolate)
#'
#' for (result in results){
#'   print(result$result)
#' }
#'
#' b1 <- plot(board_example_list[[1]])
#' b2 <- plot(board_example_list[[2]])
#' b3 <- plot(board_example_list[[3]])
#' b4 <- plot(results[[1]]$`result_board`)
#' b5 <- plot(results[[2]]$`result_board`)
#' b6 <- plot(results[[3]]$`result_board`)
#'
#' grid.arrange(b1, b2, b3, b4, b5, b6, nrow=2)
percolate.board <- function(start_board){
  assert_that(is_valid(start_board), msg="must be a valid board")

  result_board <- start_board
  n <- attr(start_board, "n")
  result_board[1,] <- ifelse(result_board[1,] == 1, 2, 0)

  percolate_helper <- function(board_start){
    board_end <- board_start
    for(row in 1:n){
      for (col in 1:n){
        if (board_end[row, col] == 2){
          if (row < n && (board_end[row+1, col] == 1)){
            board_end[row+1, col] <- 2
          }
          if (row > 1 && (board_end[row-1, col] == 1)){
            board_end[row-1, col] <- 2
          }
          if (col < n && (board_end[row, col+1] == 1)){
            board_end[row, col+1] <- 2
          }
          if (col > 1 && (board_end[row, col-1] == 1)){
            board_end[row, col-1] <- 2
          }
        }
      }
    }

    if (all.equal(board_start, board_end, check.names=FALSE, check.attributes=FALSE) == TRUE){
      return(board_end)
    } else {
      return(percolate_helper(board_end))
    }
  }

  result_board <- percolate_helper(result_board)

  result <- 2 %in% result_board[n,]

  return(list(result_board=result_board, result=result))
}
