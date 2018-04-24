##########
## Visualize 4D
##########
library(tourr)
library(tidyverse)

dummies_data <- data.frame(hor = 2:5,pen=4:7,for_hor = seq(1,12,3),obv = seq(1,4,1))

ggplot(data = dummies_data, aes(x = hor,y = pen)) + geom_point(shape = 1, fill = dummies_data$for_hor,size = dummies_data$obv/50)

ggplot(data = dummies_data,aes(x = hor,y = pen)) + geom_point(colour = dummies_data$for_hor,size = dummies_data$obv)
  
  
  geom_raster(
    aes(fill = for_hor)) + scale_fill_gradient(
      low = "red",high = "green") + ggtitle("Colour mapping of mean values 2016/2017")
