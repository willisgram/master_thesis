############
# Accuracy tests of forecasts 2016
############
library(forecast)


last_gw <- 38

#############
# Actuals
#############
points_realized <- mutate_all(points_round_16,as.numeric) %>% select(2:(last_gw+1))
actuals <- gather(data = points_realized,key = "round",value = "actuals") %>% select(actuals)

############
# Accuracy test Argentina
############
#forecasts_argentina <- read.csv(file = "load/data_16/data_16_output/forecasts_argentina.csv")
#forecasts_argentina_short <- gather(data = forecasts_argentina,key = "round",value = "forecasts") %>% select(forecasts)
#acc_argentina <- accuracy(f = forecasts_argentina_short$forecasts,x = actuals$actuals)

############
# Accuracy test Improved a = 3
############
forecasts_improved_3 <- read.csv(file = "load/data_16/data_16_output/forecasts_improved_2016_hor_3.csv")
forecasts_improved_short_3 <- gather(data = forecasts_improved_3,key = "round",value = "forecasts") %>% select(forecasts)
acc_improved_3 <- accuracy(f = forecasts_improved_short_3$forecasts,x = actuals$actuals)

############
# Accuracy test Improved a = 4
############
forecasts_improved_4 <- read.csv(file = "load/data_16/data_16_output/forecasts_improved_2016_hor_4.csv")
forecasts_improved_short_4 <- gather(data = forecasts_improved_4,key = "round",value = "forecasts") %>% select(forecasts)
acc_improved_4 <- accuracy(f = forecasts_improved_short_4$forecasts,x = actuals$actuals)

############
# Accuracy test Improved a = 5
############
forecasts_improved_5 <- read.csv(file = "load/data_16/data_16_output/forecasts_improved_2016_hor_5.csv")
forecasts_improved_short_5 <- gather(data = forecasts_improved_5,key = "round",value = "forecasts") %>% select(forecasts)
acc_improved_5 <- accuracy(f = forecasts_improved_short_5$forecasts,x = actuals$actuals)

############
# Accuracy test Improved a = 6
############
forecasts_improved_6 <- read.csv(file = "load/data_16/data_16_output/forecasts_improved_2016_hor_6.csv")
forecasts_improved_short_6 <- gather(data = forecasts_improved_6,key = "round",value = "forecasts") %>% select(forecasts)
acc_improved_6 <- accuracy(f = forecasts_improved_short_6$forecasts,x = actuals$actuals)

############
# Accuracy test Improved a = 7
############
forecasts_improved_7 <- read.csv(file = "load/data_16/data_16_output/forecasts_improved_2016_hor_7.csv")
forecasts_improved_short_7 <- gather(data = forecasts_improved_7,key = "round",value = "forecasts") %>% select(forecasts)
acc_improved_7 <- accuracy(f = forecasts_improved_short_7$forecasts,x = actuals$actuals)

############
# Accuracy test Improved a = 8
############
forecasts_improved_8 <- read.csv(file = "load/data_16/data_16_output/forecasts_improved_2016_hor_8.csv")
forecasts_improved_short_8 <- gather(data = forecasts_improved_8,key = "round",value = "forecasts") %>% select(forecasts)
acc_improved_8 <- accuracy(f = forecasts_improved_short_8$forecasts,x = actuals$actuals)

############
# Accuracy test Improved a = 10
############
forecasts_improved_10 <- read.csv(file = "load/data_16/data_16_output/forecasts_improved_2016_hor_10.csv")
forecasts_improved_short_10 <- gather(data = forecasts_improved_10,key = "round",value = "forecasts") %>% select(forecasts)
acc_improved_10 <- accuracy(f = forecasts_improved_short_10$forecasts,x = actuals$actuals)

############
# Regression no update
############

forecasts_regression_short <- forecasts_regression[2:25]
forecasts_regression_short <- gather(data = forecasts_regression_short,key = "round",value = "forecasts") %>% select(forecasts)
acc_regression <- accuracy(f = forecasts_regression_short$forecasts,x = actuals$actuals[1:625*25])

############
# Regression no update
############

forecasts_regression_short <- forecasts_regression[2:25]
forecasts_regression_short <- gather(data = forecasts_regression_short,key = "round",value = "forecasts") %>% select(forecasts)
acc_regression <- accuracy(f = forecasts_regression_short$forecasts,x = actuals$actuals[1:625*25])

