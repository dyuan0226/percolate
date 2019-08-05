library(assertthat)


#' generate_board_mat()
#'
#' @param n specifies the size of one side of the board (must be positive integer)
#' @param p specifies the proportion of the board that is blocked (between 0 an 1)
#'
#' @return an nxn board with p-proportion squares blocked (delineated by 0's and 1's)
#' @examples generate_board_mat()
#' generate_board_mat(n = 8, p = 0.75)
generate_board_mat <- function(n=5, p=0.25){
  assert_that(is.numeric(n) && is.numeric(p), msg="n and p must be numerics")
  assert_that(length(n) == 1 && length(p) == 1, msg="n and p must be single values")
  assert_that(as.integer(n) == n && n > 0, msg="n must be a positive integer")
  assert_that(0<=p && p<=1, msg="p must be between 0 and 1")
  num_tiles <- n^2
  num_blocked <- floor(p*num_tiles)
  blocked_tiles <- sample(1:num_tiles, size=num_blocked, replace=FALSE)
  matrix(ifelse(1:num_tiles %in% blocked_tiles, 0, 1), nrow=n)
}

#' is_valid()
#'
#' @param mat specifies our test variables
#'
#' @return whether or not `mat` is a valid board
#' @examples is_valid(generate_board_mat())
#' is_valid(generate_board_mat(n=1))
is_valid <- function(mat){
  assert_that(is.matrix(mat), msg="must be a matrix")
  assert_that(nrow(mat) == ncol(mat), msg="must be square")
  assert_that(all(mat %in% 0:2), msg="values must be 0, 1, 2")
  return(TRUE)
}