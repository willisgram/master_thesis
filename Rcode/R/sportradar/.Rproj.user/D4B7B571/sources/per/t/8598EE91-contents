###################
#Simple regression based on data only from overlord, filter from week 1
###################


regressors_0 <- data_0 %>% mutate(
  cost_0 = Cost, 
  tot_points_0 = TotalPoints, 
  points_last_0 = PointsLastRound, 
  transfers_in_0 = TransfersInRound,
  transfers_out_0 = TransfersOutRound,
  selected_0 = SelectedByPercent
) %>% select(cost_0,points_last_0,transfers_in_0,transfers_out_0,selected_0)

names_id <- data_1 %>% filter(Surname != "Ward") %>% mutate(
  id = 1:511
) %>% 
  select(Surname, id)



regressors_1_1 <- data_1 %>% mutate(
  cost_1 = Cost, 
  tot_points_1 = TotalPoints, 
  points_last_1 = PointsLastRound, 
  transfers_in_1 = TransfersInRound,
  transfers_out_1 = TransfersOutRound,
  selected_1 = SelectedByPercent
) %>% filter(Surname != "Ward") %>% mutate(
  id = 1:511
  ) %>% 
  select(id,cost_1,points_last_1,selected_1)

regressors_1_2 <- data_2 %>% mutate(
  cost_2 = Cost, 
  tot_points_2 = TotalPoints, 
  points_last_2 = PointsLastRound, 
  transfers_in_2 = TransfersInRound,
  transfers_out_2 = TransfersOutRound,
  selected_2 = SelectedByPercent
) %>% filter(Surname %in% data_1$Surname & FirstName %in% data_1$FirstName & Surname != "Ward") %>% mutate(
  id = 1:nrow(regressors_1_1)
) %>% 
  select(id,cost_2,points_last_2,selected_2)

regressors_1_3 <- data_3 %>% mutate(
  cost_3 = Cost, 
  tot_points_3 = TotalPoints, 
  points_last_3 = PointsLastRound, 
  transfers_in_3 = TransfersInRound,
  transfers_out_3 = TransfersOutRound,
  selected_3 = SelectedByPercent
) %>% filter(Surname %in% data_1$Surname & FirstName %in% data_1$FirstName & Surname != "Ward") %>% mutate(
  id = 1:nrow(regressors_1_1)
) %>% 
  select(id,cost_3,points_last_3,selected_3)

regressors_2_1 <- data_4 %>% mutate(
  cost_1 = Cost, 
  tot_points_1 = TotalPoints, 
  points_last_1 = PointsLastRound, 
  transfers_in_1 = TransfersInRound,
  transfers_out_1 = TransfersOutRound,
  selected_1 = SelectedByPercent
) %>% filter(Surname %in% data_1$Surname & FirstName %in% data_1$FirstName & Surname != "Ward") %>% mutate(
  id = 1:nrow(regressors_1_1)
) %>% 
  select(id,cost_1,points_last_1,selected_1)

regressors_2_2 <- data_5 %>% mutate(
  cost_2 = Cost, 
  tot_points_2 = TotalPoints, 
  points_last_2 = PointsLastRound, 
  transfers_in_2 = TransfersInRound,
  transfers_out_2 = TransfersOutRound,
  selected_2 = SelectedByPercent
) %>% filter(Surname %in% data_1$Surname & FirstName %in% data_1$FirstName & Surname != "Ward") %>% mutate(
  id = 1:nrow(regressors_1_1)
) %>% 
  select(id,cost_2,points_last_2,selected_2)

regressors_2_3 <- data_6 %>% mutate(
  cost_3 = Cost, 
  tot_points_3 = TotalPoints, 
  points_last_3 = PointsLastRound, 
  transfers_in_3 = TransfersInRound,
  transfers_out_3 = TransfersOutRound,
  selected_3 = SelectedByPercent
) %>% filter(Surname %in% data_1$Surname & FirstName %in% data_1$FirstName & Surname != "Ward") %>% mutate(
  id = 1:nrow(regressors_1_1)
) %>% 
  select(id,cost_3,points_last_3,selected_3)


#Fit model
points_1_3 <- regressors_1_3 %>% mutate(
  points = points_last_3
) %>% select(
  points,
  id
)

points_fit <- points_1_3$points

data_set <- inner_join(regressors_1_1,regressors_1_2, by = "id") %>% select(
  cost_1, 
  cost_2, 
  points_last_1, 
  points_last_2,
  selected_1,
  selected_2
  )

data_set <- cbind(points_fit,data_set)


model <- lm(points_fit ~ ., data = data_set)
summary(model)

#Test model
data_set_new <- inner_join(regressors_2_1,regressors_2_2, by = "id") %>% select(
  cost_1, 
  cost_2, 
  points_last_1, 
  points_last_2,
  selected_1,
  selected_2
)


predictions <- predict(object = model,newdata = data_set_new)

points_2_3 <- regressors_2_3 %>% mutate(
  points = points_last_3
) %>% select(
  points,
  id
)

points_test <- points_2_3$points

compare <- data.frame(Surname = names_id$Surname , prediction = predictions, realized = points_test) %>% mutate(
  gw_1 = predictions,
  ge_2 = predictions +1,
  ge_3 = predictions +2
) %>% select(Surname, gw_1,ge_2,ge_3)

forecasts <- t(compare)
write.xlsx(forecasts, "output/round_3.xlsx")


write.table(compare, "output/round_3.txt", sep="\t")
write.csv(compare, file = "output/round_3.csv")
library(xlsx)
write.xlsx(compare, "output/round_3.xlsx")


