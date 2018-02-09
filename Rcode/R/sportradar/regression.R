###################
#Combination of data for linear regression
###################


#Regression on a number of chosen (independent) variables
training_statistics_0 <- inner_join(data_train_0,players_away, by = "Surname") %>% filter( Team == "MCI")
training_statistics_1 <- inner_join(data_train_1,players_away, by = "Surname") %>% filter( Team == "MCI")
training_statistics_2 <- inner_join(data_train_2,players_away, by = "Surname") %>% filter( Team == "MCI")

regressors_0 <- training_statistics_0 %>% mutate(
  cost_0 = Cost, 
  tot_points_0 = TotalPoints, 
  points_last_0 = PointsLastRound, 
  transfers_in_0 = TransfersInRound,
  transfers_out_0 = TransfersOutRound
  ) %>% select(cost_0,tot_points_0,points_last_0,transfers_in_0,transfers_out_0)

regressors_1 <- training_statistics_1 %>% mutate(
  cost_1 = Cost, 
  tot_points_1 = TotalPoints, 
  points_last_1 = PointsLastRound, 
  transfers_in_1 = TransfersInRound,
  transfers_out_1 = TransfersOutRound
  ) %>% select(cost_1,tot_points_1,points_last_1,transfers_in_1,transfers_out_1)

regressors_2 <- training_statistics_2 %>% mutate(
  cost_2 = Cost, 
  tot_points_2 = TotalPoints, 
  points_last_2 = PointsLastRound, 
  transfers_in_2 = TransfersInRound,
  transfers_out_2 = TransfersOutRound
  ) %>% select(cost_2,tot_points_2,points_last_2,transfers_in_2,transfers_out_2)

points_fit <- data_fit %>% filter(
  Surname %in% players_away$Surname
  ) %>% filter(
    Team == "MCI"
    ) %>% mutate(
      points = PointsLastRound
      ) %>% select(
      points
      )

points_test <- data_test %>% filter(
  Surname %in% players_away$Surname
) %>% filter(
  Team == "MCI"
) %>% mutate(
  points = PointsLastRound
) %>% select(
  points
)

data_set <- bind_cols(points_fit, regressors_0,regressors_1,regressors_2)

model <- lm(points ~ ., data = data_set)
predictions <- round(predict(model))
compare <- data.frame(prediction = predictions, realized = points_test)


#Regression on a smaller number of chosen (independent) variables (does not make sense to incldude both points last and total due linear relationship)

#Create regression model
regressors_1 <- training_statistics_1 %>% mutate(
  cost_1 = Cost, 
  tot_points_1 = TotalPoints, 
  points_last_1 = PointsLastRound, 
  transfers_in_1 = TransfersInRound,
  transfers_out_1 = TransfersOutRound
) %>% select(points_last_1,transfers_in_1,transfers_out_1)

points_fit <- data_train_2 %>% filter(
  Surname %in% players_away$Surname
) %>% filter(
  Team == "MCI"
) %>% mutate(
  points = PointsLastRound
) %>% select(
  points
)

data_set <- bind_cols(points_fit,regressors_1)

model <- lm(points ~ ., data = data_set)
summary(model)


#Make predictions with new (updated) regressors:
regressors_2 <- training_statistics_2 %>% mutate(
  cost_1 = Cost, 
  tot_points_1 = TotalPoints, 
  points_last_1 = PointsLastRound, 
  transfers_in_1 = TransfersInRound,
  transfers_out_1 = TransfersOutRound
) %>% select(points_last_1,transfers_in_1,transfers_out_1)

predictions <- predict(object = model,newdata = regressors_2)


points_test <- data_test %>% filter(
  Surname %in% players_away$Surname
) %>% filter(
  Team == "MCI"
) %>% mutate(
  points = PointsLastRound
) %>% select(
  points
)






data_set <- bind_cols(points_fit,regressors_1,regressors_2)

model <- lm(points ~ ., data = data_set)
summary(model)






