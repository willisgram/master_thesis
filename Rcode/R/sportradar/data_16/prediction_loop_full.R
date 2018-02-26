#####################
# Loop to finish all prediction sheets
#####################

# index - gw_1 - gw_2 ...

# Suggestion, only change opponent and team
# Not using forecast as new previous

options(stringsAsFactors = T)

#given input
k <- 3 
observations <- dim(points_round_k_16)[1]


for(week in 6:10){

  # index-keepers
  n <- floor(week/k)
  z <- week%%k
  start <- 625*z +1
  m     <- (n*k-(1-z))*625
  
  #Forecasts begins in week 6
  
    for (j in 1:5) {
      
      
      
      if(j == 1){
        #Create regressors
        regressors_train_16_k <- regressors_16[start:m,] %>% na.omit()
        regressors_test_data_16_k  <- regressors_16[(m+1):(m+625),] %>% na.omit()
        regressors_test_data_16_k  <- regressors_test_data_16_k %>% filter(index %in% regressors_train_16_k$index)
        regressors_test_16_k  <- regressors_test_data_16_k[,names(regressors_test_data_16_k) != "realized"]
        
        ##Fit model
        model_3 <- lm(realized ~ index + prev_2 + prev_1 + opponent + team + cost + pos + trans_in_prev_2 + 
                        trans_in_prev_2 + trans_out_prev_1 + trans_out_prev_2,
                      data = regressors_train_16_k)
        
        ##Predict
        predictions <- predict(object = model_3,newdata = regressors_test_16_k)
        
        
        ##Store
        name <- paste0("gw",as.character(week))
        predictions_table <- regressors_test_data_16_k %>% mutate(
          index_int = as.integer(index)
        ) %>% select(index_int)
        
        predictions_table <- cbind(predictions_table,predictions)
        colnames(predictions_table)[j+1] <- name
        
      } else{
        
        # u for update:
        observations_u <- observations - j*625
        
        
        opponent_round_k_16_u <- opponent_round_k_16 %>% tail(observations_u)
        team_round_k_16_u <- team_round_k_16 %>% tail(observations_u)
        
        # Static
        points_round_k_16_u <- points_round_k_16 %>% head(observations_u)
        cost_round_k_16_u <- cost_round_k_16 %>% head(observations_u)
        pos_round_k_16_u <- pos_round_k_16 %>% head(observations_u)
        trans_in_round_k_16_u <- trans_in_round_k_16 %>% head(observations_u)
        trans_out_round_k_16_u <- trans_out_round_k_16 %>% head(observations_u)
        
        
        regressors_16_u <- cbind(points_round_k_16_u,opponent_round_k_16_u,team_round_k_16_u,cost_round_k_16_u,
                                 pos_round_k_16_u,trans_in_round_k_16_u,trans_out_round_k_16_u)
        regressors_16_u$index <- as.factor(regressors_16_u$index)
        
        
        regressors_train_16_k_u <- regressors_16_u[1:m,] %>% na.omit()
        regressors_test_data_16_k_u  <- regressors_16_u[(m+1):(m+625),] %>% na.omit()
        regressors_test_data_16_k_u  <- regressors_test_data_16_k_u %>% filter(index %in% regressors_train_16_k_u$index)
        regressors_test_16_k_u  <- regressors_test_data_16_k_u[,names(regressors_test_data_16_k) != "realized"]
        
        ##New predictions
        predictions_u <- predict(object = model_3,newdata = regressors_test_16_k_u)
        
        ##Store
        ##Store
        name <- paste0("gw",as.character(week+j-1))
        predictions_table <- cbind(predictions_table,predictions_u)
        colnames(predictions_table)[j+1] <- name
      }
    }
  
  name <- paste0("forecasts_week_", as.character(week))
  assign(x = name,value = predictions_table)


}
