
#################
# Create regressors DF
#################

regressors_16 <- cbind(points_round_k_16,opponent_round_k_16,team_round_k_16,cost_round_k_16,
                       pos_round_k_16,trans_in_round_k_16,trans_out_round_k_16)
regressors_16$index <- as.factor(regressors_16$index)

#Needs generalization
regressors_train_16 <- regressors_16[1:6250,] %>% na.omit()
regressors_test_data_16  <- regressors_16[6251:6875,] %>% na.omit()
regressors_test_data_16  <- regressors_test_data_16 %>% filter(index %in% regressors_train_16$index)
regressors_test_16  <- regressors_test_data_16[,names(regressors_test_data_16) != "realized"]

#################
# Fit regression model
#################

##Fit model
options(stringsAsFactors = T)

model_3 <- lm(realized ~ index + prev_2 + prev_1 + opponent + team + cost + pos + trans_in_prev_2 + 
                trans_in_prev_2 + trans_out_prev_1 + trans_out_prev_2,
              data = regressors_train_16)
summary(model_3)


##Predict (to find best out-of-sample fit)
predictions <- predict(object = model_3,newdata = regressors_test_16)

compare <- data.frame(pred = predictions, real = regressors_test_data_16$realized )





