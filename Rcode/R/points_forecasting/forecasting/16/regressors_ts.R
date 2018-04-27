#######################
# Test for acf in points
#######################
library(forecast)

points_1 <- points_round_16 %>% filter(index == 1)
points_1 <- points_1[,names(points_1) != "index"]

points_1_ts <- ts(data = t(points_1))

Acf(x = points_1_ts)

points_9 <- points_round_16 %>% filter(index == 9)
points_9 <- points_9[,names(points_9) != "index"]

points_9_ts <- ts(data = t(points_9))

Acf(x = points_9_ts)
