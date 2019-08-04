library(assertthat)


#' board(): used as class constructor
#'
#' @param mat optional, requires valid board structure if given
#' @param n must be a positive integer, specifies size of the board
#' @param p specifies proportion of board blocked, must be between 0 and 1
#'
#' @return valid board object with attributes n and p
board <- function(mat=NULL, n=5, p=0.25){
  if (!is.null(mat)){
    assert_that(is_valid(mat), msg="input mat must be a valid board")
    class(mat) <- c("board", "matrix")
    attr(mat, "n") <- nrow(mat)
    attr(mat, "p") <- sum(mat == 0)/length(mat)
    return(mat)
  } else {
    assert_that(is.numeric(n) && is.numeric(p), msg="n and p must be numerics")
    assert_that(length(n) == 1 && length(p) == 1, msg="n and p must be single values")
    assert_that(as.integer(n) == n && n > 0, msg="n must be a positive integer")
    assert_that(0<=p && p<=1, msg="p must be between 0 and 1")
    
    mat2 <- generate_board_mat(n=n, p=p)
    class(mat2) <- c("board", "matrix")
    attr(mat2, "n") <- n
    attr(mat2, "p") <- p
    return(mat2)
  }
}

#' plot.board(), plotting function for valid boards
#'
#' @param x specifies the board to be plotted; must be a valid board object
#'
#' @return the plot of the board x
plot.board <- function(x){
  assert_that(is_valid(x), msg="x must be a valid board")
  assert_that("board" %in% class(x))
  n <- attr(x, "n")
  board_df <- data.frame(row=as.vector(row(x)), col=as.vector(col(x)), val=as.vector(x))
  board_df$val <- factor(board_df$val)
  title <- paste(n, "x", n, " board", sep="")
  ggplot(data=board_df, mapping=aes(x=row, y=col, fill=val)) + 
    geom_tile() + 
    labs(title=title) +
    theme_void() +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
    scale_fill_manual(values=c("0"="black", "1"="white", "2"="lightblue3"))
}