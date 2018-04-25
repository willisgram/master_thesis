##########
## Visualize 4D
##########
library(tourr)
library(tidyverse)


penalty_horizon_ov_short <- penalty_horizon_ov %>% group_by(penalty) %>% filter(mean == max(mean)) %>% arrange(penalty)



ggplot(data = penalty_horizon_ov_short,aes(x = f_horizon,y = penalty,colour = mean)) + geom_point(
  aes(size = horizon_fac)) + scale_color_gradient (low = "red",high = "green") + theme_classic()
