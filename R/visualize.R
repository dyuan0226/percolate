library(assertthat)
library(tidyverse)


#' plot.board(), plotting function for valid boards
#'
#' @param x specifies the board to be plotted; must be a valid board object
#'
#' @return the plot of the board x
#'
#' @export
#' @examples board_example <- board(matrix(c(0,1,1,1,0,
#' 0,1,1,0,1,
#' 0,0,1,0,0,
#' 0,0,0,2,2,
#' 2,2,2,2,0), 5, 5))
#' plot.board(board_example)
plot.board <- function(x){
  is_valid(x)
  assert_that(is_valid(x), msg="x must be a valid board")
  assert_that("board" %in% class(x))
  n <- attr(x, "n")
  board_df <- data.frame(row=as.vector(row(x)), col=as.vector(col(x)), val=as.vector(x))
  board_df$val <- factor(board_df$val)
  title <- paste("Size:", n)
  ggplot(data=board_df, mapping=aes(x=row, y=col, fill=val)) +
    geom_tile() +
    labs(title=title) +
    theme_void() +
    theme(legend.position="none",
          plot.title=element_text(hjust=0.5)) +
    scale_fill_manual(values=c("0"="black", "1"="white", "2"="lightblue3")) +
    scale_x_reverse() +
    coord_flip()
}
