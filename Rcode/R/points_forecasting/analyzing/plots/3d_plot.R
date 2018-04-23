################
# 3D plot
###############
library(tidyverse)
library(RColorBrewer)



penalty_horizon_scale <- ggplot(
  data = penalty_horizon_ov,aes(x = horizon,y = penalty)) + geom_raster(
    aes(fill = mean)) + scale_fill_gradient(
      low = "red",high = "green") + ggtitle("Colour mapping of mean values 2016/2017")

name_plot <- paste0("pen_hor_scale.png")
path_plot <- '../../../../latex_master_thesis/fig/chapter_5/'
file_plot <- paste0(path_plot, name_plot)

ggsave(file_plot)

