
#################
# Create regressors DF
#################

regressors_16 <- cbind(points_round_k_16,opponent_round_k_16,team_round_k_16,cost_round_k_16,
                       pos_round_k_16,trans_in_round_k_16,trans_out_round_k_16)
regressors_16$index <- as.factor(regressors_16$index)

#Needs generalization
#regressors_train_16 <- regressors_16[1:6250,] %>% na.omit()
#regressors_test_data_16  <- regressors_16[6251:6875,] %>% na.omit()
#regressors_test_data_16  <- regressors_test_data_16 %>% filter(index %in% regressors_train_16$index)
#regressors_test_16  <- regressors_test_data_16[,names(regressors_test_data_16) != "realized"]


#####
# Dont trust 
#####
#Generalization:
#Forecast week 9
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


#Forecast week 7 (still only "2" because there are not enough data for 3)
k <- 3
n <- 2
z <- 2

start <- 625*(z-1) + 1
m <- (n*k-(2-z))*625


regressors_train_16_k <- regressors_16[start:m,] %>% na.omit()
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

#Forecast week 8 (still only "2" because there are not enough data for 3)
k <- 3
n <- 2
z <- 3

start <- 625*(z-1) + 1
m <- (n*k-(2-z))*625


regressors_train_16_k <- regressors_16[start:m,] %>% na.omit()
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

#Forecast week 9 (Now n = 3)
k <- 3 #given input
n <- 3 #counting variable
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
