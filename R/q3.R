library(tidyverse)


test_n <- seq(10, 100, by=10)

perc_time_avg <- function(n, p=0.4, trials=10){
  times <- vector()
  for (i in 1:trials){
    my_board <- board(generate_board_mat(n=n, p=p))
    trial_time <- system.time(percolate(my_board))[3]
    times <- c(times, trial_time)
  }
  return(mean(times))
}

avg_times <- sapply(test_n, FUN=perc_time_avg)
time_df <- data.frame(size=test_n^2, avg_time=avg_times)
ggplot(data=time_df, aes(x=size, y=avg_time)) + 
  geom_line() + 
  labs(title="Average Runtime by Size of Board", x="Size of Board (n^2)", y="Average Runtime")

time_lm <- lm(avg_time ~ size, data=time_df)

ggplot(data=time_df, aes(x=size, y=avg_time)) + 
  geom_line() + 
  labs(title="Average Runtime by Size of Board", x="Size of Board (n^2)", y="Average Runtime") + 
  geom_abline(color="red", slope=coef(time_lm)["size"], intercept=coef(time_lm)["(Intercept)"])

test_p <- seq(0, 1, by=0.05)

perc_percentage <- function(n, p, trials=20){
  successes <- 0
  for (i in 1:trials){
    my_board <- board(generate_board_mat(n=n, p=p))
    if (percolate(my_board)$result){
      successes <- successes + 1
    }
  }
  return(successes/trials)
}

n_5_percentage <- sapply(test_p, FUN=perc_percentage, n=5)
n_10_percentage <- sapply(test_p, FUN=perc_percentage, n=10)
n_25_percentage <- sapply(test_p, FUN=perc_percentage, n=25)
percentage_df <- data.frame(p=test_p, n5=n_5_percentage, n10=n_10_percentage, n25=n_25_percentage) %>% 
  gather(key = "size", value = "percentage", 2:4) %>% 
  mutate(size=factor(size, levels=unique(percentage_df$size)))

ggplot(data=percentage_df, aes(x=p, y=percentage, color=size)) + 
  geom_line() + 
  scale_fill_manual(values=c("blue", "red", "black")) + 
  labs(title="Percentage of Percolating Boards by Percentage of Squares Blocked", 
       x="Percentage of Squares Blocked",
       y="Percentage of Boards Percolating") +
  scale_color_manual(values=c("black", "red", "blue")) + 
  scale_x_continuous(breaks=seq(0, 1, by=0.2))