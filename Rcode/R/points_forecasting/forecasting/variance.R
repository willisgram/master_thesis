####################
# Script for finding the variance of each player
#####################
library(tidyverse)


points_round_16_t <- t(points_round_16)
var_16 <- data.frame(index = 1:625,var=rep(0,625))

for( i in 1:625){
  var_16$var[i] <- var(points_round_16_t[2:39,i])
}



