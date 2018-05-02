##########
## Visualize 4D, another possibility is to make 1 plot for each f_horizon
##########
library(tourr)
library(tidyverse)

penalty_horizon_ov_3_7 <- read.csv(file = "load/data_16/data_16_output/penalty_horizon_ov_3_7.csv")

penalty_horizon_ov_short <- penalty_horizon_ov %>% group_by(penalty) %>% filter(mean == max(mean)) %>% arrange(penalty)
penalty_horizon_ov_short$horizon <- as.factor(penalty_horizon_ov_short$horizon)
penalty_horizon_ov_short$f_horizon <- as.factor(penalty_horizon_ov_short$f_horizon)


ggplot(data = penalty_horizon_ov_short,aes(x = f_horizon,y = penalty,colour = mean)) + geom_point(
  aes(size = horizon)) + scale_color_gradient (low = "red",high = "green") + theme_classic()
