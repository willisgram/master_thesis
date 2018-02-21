#############################
# The aim is to generate a generizeable regression model players contained in players_0
# for week 8
# The training set is 0-7
# The regression,for now, is "non-player specific"
#############################

############
#Data wash
############
# Regressors 1
#############
regressors_1 <- regressors_1 %>% filter(index %in% regressors_1$index)
regressors_2 <- regressors_2 %>% filter(index %in% regressors_1$index)
regressors_3 <- regressors_3 %>% filter(index %in% regressors_1$index)
regressors_4 <- regressors_4 %>% filter(index %in% regressors_1$index)
regressors_5 <- regressors_5 %>% filter(index %in% regressors_1$index)
regressors_6 <- regressors_6 %>% filter(index %in% regressors_1$index)
regressors_7 <- regressors_7 %>% filter(index %in% regressors_1$index)
regressors_8 <- regressors_8 %>% filter(index %in% regressors_1$index)
regressors_9 <- regressors_9 %>% filter(index %in% regressors_1$index)
regressors_9 <- regressors_9 %>% filter(index %in% regressors_1$index)
regressors_10 <- regressors_10 %>% filter(index %in% regressors_1$index)
regressors_11 <- regressors_11 %>% filter(index %in% regressors_1$index)
regressors_12 <- regressors_12 %>% filter(index %in% regressors_1$index)
regressors_11 <- regressors_11 %>% filter(index %in% regressors_1$index)



regressors_tot_1 <-  regressors_3 %>% mutate(
  points_3 = regressors_1$points,
  points_2 = regressors_2$points,
  points_1 = points,
  next_match = regressors_4$opponent,
  points_real = regressors_4$points
) %>% select(Team,PositionsList,Cost,TransfersInRound,TransfersOutRound,points_3,points_2,points_1,next_match,points_real)


# Regressors 2
#############
regressors_5 <- regressors_5 %>% filter(index %in% regressors_1$index)
regressors_6 <- regressors_6 %>% filter(index %in% regressors_1$index)
regressors_7 <- regressors_7 %>% filter(index %in% regressors_1$index)
regressors_8 <- regressors_8 %>% filter(index %in% regressors_1$index)


regressors_tot_2 <-  regressors_7 %>% mutate(
  points_3 = regressors_5$points,
  points_2 = regressors_6$points,
  points_1 = points,
  next_match = regressors_8$opponent,
  points_real = regressors_8$points
) %>% select(Team,PositionsList,Cost,TransfersInRound,TransfersOutRound,points_3,points_2,points_1,next_match,points_real)

# Predict
#############
regressors_9  <- regressors_9 %>% filter(index %in% regressors_1$index)
regressors_10 <- regressors_10 %>% filter(index %in% regressors_1$index)
regressors_11 <- regressors_11 %>% filter(index %in% regressors_1$index)
regressors_12 <- regressors_12 %>% filter(index %in% regressors_1$index)


predictors <-  regressors_11 %>% mutate(
  points_3 = regressors_9$points,
  points_2 = regressors_10$points,
  points_1 = points,
  next_match = regressors_12$opponent
) %>% select(Team,PositionsList,Cost,TransfersInRound,TransfersOutRound,points_3,points_2,points_1,next_match)


regressors_tot <- rbind(regressors_tot_1,regressors_tot_2)


##########################
# Regression
##########################
options(stringsAsFactors = T)
model_3 <- lm(points_real ~ ., data = regressors_tot)
summary(model_3)

###########################
# Prediction
###########################

predictions <- predict(object = model_3,newdata = predictors)

compare <- data.frame(predictions = round(predictions), real = regressors_12$points)


