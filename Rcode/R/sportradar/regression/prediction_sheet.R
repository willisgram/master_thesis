#####################
# Creation of a finished predictions sheet
#####################

# index - gw_1 - gw_2 ...

# Suggestion, only change opponent
# Use forecast as new previous

#Forecast week 6
k <- 3 #given input
n <- 2 #counting variable
z <- 1 #counting variable

#index variables
start <- 625*(z-1) + 1
m <- (n*k-(2-z))*625

regressors_train_16_k <- regressors_16[1:m,] %>% na.omit()
regressors_test_data_16_k  <- regressors_16[(m+1):(m+625),] %>% na.omit()
regressors_test_data_16_k  <- regressors_test_data_16_k %>% filter(index %in% regressors_train_16_k$index)
regressors_test_16_k  <- regressors_test_data_16_k[,names(regressors_test_data_16_k) != "realized"]


#################
# Fit regression model
#################

##Fit model
options(stringsAsFactors = T)

model_3 <- lm(realized ~ index + prev_2 + prev_1 + opponent + team + cost + pos + trans_in_prev_2 + 
                trans_in_prev_2 + trans_out_prev_1 + trans_out_prev_2,
              data = regressors_train_16_k)
summary(model_3)


##Predict (to find best out-of-sample fit)
predictions <- predict(object = model_3,newdata = regressors_test_16_k)

compare <- regressors_test_data_16_k %>% select(index, realized) %>% mutate(
  predictions = predictions
)

#### step = 2:

# Update:
opponent_round_k_16_u <- opponent_round_k_16 %>% tail(6250)
team_round_k_16_u <- team_round_k_16 %>% tail(6250)

# Static
points_round_k_16_u <- points_round_k_16 %>% head(6250)
cost_round_k_16_u <- cost_round_k_16 %>% head(6250)
pos_round_k_16_u <- pos_round_k_16 %>% head(6250)
trans_in_round_k_16_u <- trans_in_round_k_16 %>% head(6250)
trans_out_round_k_16_u <- trans_out_round_k_16 %>% head(6250)


regressors_16_u <- cbind(points_round_k_16_u,opponent_round_k_16_u,team_round_k_16_u,cost_round_k_16_u,
                       pos_round_k_16_u,trans_in_round_k_16_u,trans_out_round_k_16_u)
regressors_16_u$index <- as.factor(regressors_16_u$index)


regressors_train_16_k_u <- regressors_16_u[1:m,] %>% na.omit()
regressors_test_data_16_k_u  <- regressors_16_u[(m+1):(m+625),] %>% na.omit()
regressors_test_data_16_k_u  <- regressors_test_data_16_k_u %>% filter(index %in% regressors_train_16_k_u$index)
regressors_test_16_k_u  <- regressors_test_data_16_k_u[,names(regressors_test_data_16_k) != "realized"]

##New predictions
predictions_u <- predict(object = model_3,newdata = regressors_test_16_k_u)

predictions_table <- regressors_test_data_16_k %>% select(index) %>% mutate(
  gw_1 = predictions,
  gw_2 = predictions_u
)


compare <- regressors_test_data_16_k %>% select(index, realized) %>% mutate(
  predictions = predictions
)



