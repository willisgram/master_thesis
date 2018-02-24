#################
# Create predictors DF
#################

#Needs generalization
predictors  <- regressors_17[1:625,] %>% na.omit()
predictors  <- predictors %>% filter(index %in% regressors_train$index) %>% filter(
  !opponent %in% c("Brighton", "Huddersfield", "Newcastle")) %>% filter(
    !team %in% c("BHA", "HUD", "NEW")
    )

realizations <- predictors$realized
predictors  <- predictors[,names(predictors) != "realized"]



##Predict
predictions <- predict(object = model_3,newdata = predictors)

compare <- data.frame(pred = round(predictions), real = realizations )
