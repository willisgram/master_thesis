############################
# Regressors selection
############################

library(leaps)
library(leaps)

start <- 1
m <- 625*5

regressors_train_16 <- regressors_16[start:m,] %>% na.omit()
regressors_test_data_16  <- regressors_16[(m+1):(m+625),] %>% na.omit()
regressors_test_data_16  <- regressors_test_data_16 %>% filter(index %in% regressors_train_16_k$index)
regressors_test_16  <- regressors_test_data_16[,names(regressors_test_data_16_k) != "realized"]



options(stringsAsFactors = T)

reg1 <- regsubsets(realized ~  prev_1 + opponent,
                   data = regressors_train_16, nvmax = 4)

reg1 <- regsubsets(realized ~ index + prev_2 + prev_1 + opponent + team + cost + pos + trans_in_prev_2 + 
                     trans_in_prev_2 + trans_out_prev_1 + trans_out_prev_2,
                   data = regressors_train_16, nvmax = 4, really.big = T)

###############
# Regression on 1 player
#################

regressors_train_16_1 <- regressors_train_16 %>% filter(index %in% seq(from = 1,to = 625,by = 20))
#regressors_train_16_1 <- regressors_train_16_1[,names(regressors_train_16_1) != "index"]

reg1 <- regsubsets(realized ~ index + prev_2 + prev_1 + opponent + team + cost + pos + trans_in_prev_2 + 
                     trans_in_prev_2 + trans_out_prev_1 + trans_out_prev_2,
                   data = regressors_train_16_1, nvmax = 4)






