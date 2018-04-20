############
# Accuracy tests of forecasts
############
library(forecast)


last_gw <- 29

#############
# Actuals
#############
points_realized <- mutate_all(points_round_17,as.numeric) %>% select(2:(last_gw+1))
actuals <- gather(data = points_realized,key = "round",value = "actuals") %>% select(actuals)

############
# Accuracy test Argentina
############
forecasts_argentina <- read.csv(file = "load/data_17/data_17_output/forecasts_argentina.csv")
forecasts_argentina_short <- gather(data = forecasts_argentina,key = "round",value = "forecasts") %>% select(forecasts)
acc_argentina <- accuracy(f = forecasts_argentina_short$forecasts,x = actuals$actuals)

############
# Accuracy test Improved
############
forecasts_improved <- read.csv(file = "load/data_17/data_17_output/forecasts_improved.csv")
forecasts_improved_short <- gather(data = forecasts_improved,key = "round",value = "forecasts") %>% select(forecasts)
acc_improved <- accuracy(f = forecasts_improved_short$forecasts,x = actuals$actuals)

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

