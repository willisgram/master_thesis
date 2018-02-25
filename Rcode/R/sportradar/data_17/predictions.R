#################
# Create regressors DF
#################

regressors_17 <- cbind(points_round_k_17,opponent_round_k_17,team_round_k_17,
                       cost_round_k_17, pos_round_k_17,trans_in_round_k_17,trans_out_round_k_17)
regressors_17$index <- as.factor(regressors_17$index)

#Needs generalization
#regressors_train <- regressors[1:6250,] %>% na.omit()
#regressors_test_data  <- regressors[6251:6875,] %>% na.omit()
#regressors_test_data  <- regressors_test_data %>% filter(index %in% regressors_train$index)
#regressors_test  <- regressors_test_data[,names(regressors_test_data) != "realized"]

#################
# Create predictors DF
#################

#Needs generalization
predictors  <- regressors_17[1:625,] %>% na.omit()
predictors  <- predictors %>% filter(index %in% regressors_train_16$index) %>% filter(
  !opponent %in% c("Brighton", "Huddersfield", "Newcastle")) %>% filter(
    !team %in% c("BHA", "HUD", "NEW")
    )

realizations <- predictors$realized
predictors  <- predictors[,names(predictors) != "realized"]



##Predict
predictions <- predict(object = model_3,newdata = predictors)

compare <- data.frame(pred = predictions, real = realizations )
