####################################
# Dummy files for forecasts and cost
####################################

#Create dummy forecasts
forecasts_0 <- players %>% mutate(
  gw_1 = runif(n = 625,min = 0,max = 16),
  gw_2 = runif(n = 625,min = 0,max = 16),
  gw_3 = runif(n = 625,min = 0,max = 16),
  gw_4 = runif(n = 625,min = 0,max = 16),
  gw_5 = runif(n = 625,min = 0,max = 16)
) %>% select(index,gw_1,gw_2,gw_3,gw_4,gw_5)

forecasts_1 <- players %>% mutate(
  gw_1 = runif(n = 625,min = 0,max = 16),
  gw_2 = runif(n = 625,min = 0,max = 16),
  gw_3 = runif(n = 625,min = 0,max = 16),
  gw_4 = runif(n = 625,min = 0,max = 16),
  gw_5 = runif(n = 625,min = 0,max = 16)
) %>% select(index,gw_1,gw_2,gw_3,gw_4,gw_5)

forecasts_2 <- players %>% mutate(
  gw_1 = runif(n = 625,min = 0,max = 16),
  gw_2 = runif(n = 625,min = 0,max = 16),
  gw_3 = runif(n = 625,min = 0,max = 16),
  gw_4 = runif(n = 625,min = 0,max = 16),
  gw_5 = runif(n = 625,min = 0,max = 16)
) %>% select(index,gw_1,gw_2,gw_3,gw_4,gw_5)


#Create cost matrix
players_cost_0 <- players_0 %>% select(index,Cost)
players_cost_0[is.na(players_cost_0)] <- 9999999999


cost_0 <- players_cost_0 %>% mutate(
  cost_1 = Cost,
  cost_2 = Cost,
  cost_3 = Cost,
  cost_4 = Cost,
  cost_5 = Cost
) %>% select(index,cost_1,cost_2,cost_3,cost_4,cost_5)

players_cost_1 <- players_1 %>% select(index,Cost)
players_cost_1[is.na(players_cost_1)] <- 9999999999

cost_1 <- players_cost_1 %>% mutate(
  cost_1 = Cost,
  cost_2 = Cost,
  cost_3 = Cost,
  cost_4 = Cost,
  cost_5 = Cost
) %>% select(index,cost_1,cost_2,cost_3,cost_4,cost_5)

players_cost_2 <- players_2 %>% select(index,Cost)
players_cost_2[is.na(players_cost_2)] <- 9999999999

cost_2 <- players_cost_2 %>% mutate(
  cost_1 = Cost,
  cost_2 = Cost,
  cost_3 = Cost,
  cost_4 = Cost,
  cost_5 = Cost
) %>% select(index,cost_1,cost_2,cost_3,cost_4,cost_5)


library(xlsx)

path_base <- "\\\\sambaad.stud.ntnu.no/williaei/Documents/GitHub/master_thesis/dynamic_data/"
path_forecast_0 <- paste0(path_base,"forecast_0.xlsx")
path_cost_0     <- paste0(path_base,"cost_0.xlsx")

write.xlsx(forecasts_0, file = path_forecast_0,row.names = F)
write.xlsx(cost_0, file = path_cost_0,row.names = F)

path_base <- "\\\\sambaad.stud.ntnu.no/williaei/Documents/GitHub/master_thesis/dynamic_data/"
path_forecast_1 <- paste0(path_base,"forecast_1.xlsx")
path_cost_1     <- paste0(path_base,"cost_1.xlsx")

write.xlsx(forecasts_1, file = path_forecast_1,row.names = F)
write.xlsx(cost_1, file = path_cost_1,row.names = F)

path_base <- "\\\\sambaad.stud.ntnu.no/williaei/Documents/GitHub/master_thesis/dynamic_data/"
path_forecast_2 <- paste0(path_base,"forecast_2.xlsx")
path_cost_2     <- paste0(path_base,"cost_2.xlsx")

write.xlsx(forecasts_2, file = path_forecast_2,row.names = F)
write.xlsx(cost_2, file = path_cost_2,row.names = F)




