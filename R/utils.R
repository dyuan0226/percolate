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

#' string_to_board() a helper function to convert string boards to actual boards if possible, or NA if not
#'
#' @param list_mat a list specifying the size and content of the board to be created
#'
#' @return either a matrix numeric version of the board or NA
string_to_board <- function(list_mat){
  assert_that(is.list(list_mat))
  assert_that(length(list_mat) == 2)
  assert_that("n" %in% names(list_mat) && "mat" %in% names(list_mat))
  assert_that(is.numeric(list_mat$n))
  
  n <- list_mat$n
  str <- list_mat$mat
  mat <- matrix(nrow=n, ncol=n)
  str <- unlist(strsplit(str, split=""))
  assert_that(length(str) == length(mat))
  for (i in 1:length(mat)){
    if (str[i] == "."){
      mat[i] <- 1
    } else if (str[i] == "*"){
      mat[i] <- 0
    } else {
      return(NA)
    }
  }
  return(board(mat))
}

#' mat_list_to_board() function that converts list with sizes and string matrices to list of boards
#'
#' @param mat_list has two components of equivalent length: 
#' n, which specifies the size of each board and 
#' mat, which is a string version of each board (of length n^2, for the corresponding n)
#'
#' @return a list of boards converted from the strings, or NA if incorrectly formatted
mat_list_to_board <- function(mat_list){
  assert_that(is.list(mat_list))
  
  return(lapply(mat_list, FUN=string_to_board))
}

#' scrape_mat() takes a string with n and string matrix (separated by new lines) and returns a list containing n and mat
#'
#' @param mat_obj in the format of n (a positive integer), followed by the string matrix
#'
#' @return a list containing n and mat
scrape_mat <- function(mat_obj){
  lines <- mat_obj %>% strsplit(split="\n") %>% unlist
  n <- as.numeric(lines[2])
  assert_that(!is.na(n), msg="incorrectly formatted")
  assert_that(n > 0 && floor(n) == n, msg="n must be positive integer")
  return(list(n=as.numeric(lines[2]), mat=paste(lines[c(-1, -2)], collapse="")))
}

#' read_boards() function for reading from a txt file to a list of boards
#'
#' @param file specifies the path of the input file
#'
#' @return a list of boards as specified inside the file
#' @examples read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_example.txt")
read_boards <- function(file){
  readLines(file) %>% 
    .[which(.!="")] %>% 
    paste(collapse="\n") %>% 
    strsplit(split="") %>% 
    unlist %>% 
    .[which(. != " ")] %>% 
    paste(collapse="") %>% 
    strsplit(split="----") %>% 
    unlist %>% 
    .[which(.!="")] %>% 
    lapply(FUN=scrape_mat) %>% 
    mat_list_to_board
}
