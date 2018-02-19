###################
#Simple regression based on data from overlord
###################

#Create model using data from round 1 to predict round 2
#Test model by using data from round 3 to predict round 4

statistics_0 <- inner_join(data_0,players_away, by = "Surname") %>% filter( Team == "MCI")
statistics_1 <- inner_join(data_1,players_away, by = "Surname") %>% filter( Team == "MCI")
statistics_2 <- inner_join(data_2,players_away, by = "Surname") %>% filter( Team == "MCI")
statistics_3 <- inner_join(data_3,players_away, by = "Surname") %>% filter( Team == "MCI")
statistics_4 <- inner_join(data_4,players_away, by = "Surname") %>% filter( Team == "MCI")
statistics_5 <- inner_join(data_4,players_away, by = "Surname") %>% filter( Team == "MCI")
statistics_6 <- inner_join(data_4,players_away, by = "Surname") %>% filter( Team == "MCI")



#Create regressors
regressors_0 <- statistics_0 %>% mutate(
  cost_0 = Cost, 
  tot_points_0 = TotalPoints, 
  points_last_0 = PointsLastRound, 
  transfers_in_0 = TransfersInRound,
  transfers_out_0 = TransfersOutRound,
  selected_0 = SelectedByPercent
) %>% select(cost_0,points_last_0,transfers_in_0,transfers_out_0,selected_0)

regressors_1_1 <- statistics_1 %>% mutate(
  cost_1 = Cost, 
  tot_points_1 = TotalPoints, 
  points_last_1 = PointsLastRound, 
  transfers_in_1 = TransfersInRound,
  transfers_out_1 = TransfersOutRound,
  selected_1 = SelectedByPercent
) %>% select(cost_1,points_last_1,selected_1)

regressors_2_1 <- statistics_2 %>% mutate(
  cost_2 = Cost, 
  tot_points_2 = TotalPoints, 
  points_last_2 = PointsLastRound, 
  transfers_in_2 = TransfersInRound,
  transfers_out_2 = TransfersOutRound,
  selected_2 = SelectedByPercent
) %>% select(cost_2,points_last_2,selected_2)

regressors_3_1 <- statistics_3 %>% mutate(
  cost_3 = Cost, 
  tot_points_3 = TotalPoints, 
  points_last_3 = PointsLastRound, 
  transfers_in_3 = TransfersInRound,
  transfers_out_3 = TransfersOutRound,
  selected_3 = SelectedByPercent
) %>% select(cost_3,points_last_3,selected_3)

regressors_1_2 <- statistics_1 %>% mutate(
  cost_1 = Cost, 
  tot_points_1 = TotalPoints, 
  points_last_1 = PointsLastRound, 
  transfers_in_1 = TransfersInRound,
  transfers_out_1 = TransfersOutRound,
  selected_1 = SelectedByPercent
) %>% select(cost_1,points_last_1,selected_1)

regressors_2_2 <- statistics_2 %>% mutate(
  cost_2 = Cost, 
  tot_points_2 = TotalPoints, 
  points_last_2 = PointsLastRound, 
  transfers_in_2 = TransfersInRound,
  transfers_out_2 = TransfersOutRound,
  selected_2 = SelectedByPercent
) %>% select(cost_2,points_last_2,selected_2)

regressors_3_2 <- statistics_3 %>% mutate(
  cost_3 = Cost, 
  tot_points_3 = TotalPoints, 
  points_last_3 = PointsLastRound, 
  transfers_in_3 = TransfersInRound,
  transfers_out_3 = TransfersOutRound,
  selected_3 = SelectedByPercent
) %>% select(cost_3,points_last_3,selected_3)


#Fit model

points_3_1 <- data_3 %>% filter(
  Surname %in% players_away$Surname
) %>% filter(
  Team == "MCI"
) %>% mutate(
  points = PointsLastRound
) %>% select(
  points
)
data_set <- bind_cols(points_3_1,regressors_1_1,regressors_2_1)

model <- lm(points ~ ., data = data_set)
summary(model)

#Test model
data_set_new <- bind_cols(regressors_1_2,regressors_2_2)

predictions <- predict(object = model,newdata = data_set_new)

realized_points <- data_6 %>% filter(
  Surname %in% players_away$Surname
) %>% filter(
  Team == "MCI"
) %>% mutate(
  points = PointsLastRound
) %>% select(
  points
)

compare <- data.frame(prediction = predictions, realized = realized_points)


write.table(compare, "output/round_3.txt", sep="\t")
write.csv(compare, file = "output/round_3.csv")
library(xlsx)
write.xlsx(compare, "output/round_3.xlsx")


