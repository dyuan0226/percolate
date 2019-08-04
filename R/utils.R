library(assertthat)
library(testthat)
library(tidyverse)







#' generate_board_mat
#'
#' @param n specifies the size of one side of the board (must be positive integer)
#' @param p specifies the proportion of the board that is blocked (between 0 an 1)
#'
#' @return an nxn board with p-proportion squares blocked (delineated by 0's and 1's)
#' @examples generate_board_mat():
#'      [,1] [,2] [,3] [,4] [,5]
#' [1,]    0    0    0    0    0
#' [2,]    1    0    1    0    1
#' [3,]    0    0    0    0    0
#' [4,]    1    0    0    0    0
#' [5,]    0    1    1    0    0
#' 
#' generate_board_mat(n = 8, p = 0.75):
#' 
#'      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#' [1,]    1    1    1    0    1    1    1    1
#' [2,]    1    0    0    1    1    1    0    1
#' [3,]    1    1    1    1    0    0    1    1
#' [4,]    1    0    1    1    1    0    1    1
#' [5,]    0    0    1    1    0    1    1    1
#' [6,]    1    1    1    0    1    0    1    1
#' [7,]    1    1    1    1    1    1    1    1
#' [8,]    0    1    1    1    1    0    0    1
generate_board_mat <- function(n=5, p=0.25){
  assert_that(as.integer(n) == n && n > 0, msg="n must be a positive integer")
  assert_that(0<=p && p<=1, msg="p must be between 0 and 1")
  num_tiles <- n^2
  num_blocked <- floor(p*num_tiles)
  blocked_tiles <- sample(1:num_tiles, size=num_blocked, replace=FALSE)
  matrix(ifelse(1:num_tiles %in% blocked_tiles, 1, 0), nrow=n)
}
