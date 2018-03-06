#####################
# Loop to finish all prediction sheets
#####################

# index - gw_1 - gw_2 ...

# Suggestion, only change opponent and team
# Not using forecast as new previous

options(stringsAsFactors = T)

#given input
k <- 3 
#observations <- dim(points_round_k_16)[1]


for(week_for in 16:16){

  # index-keepers
  n <- floor(week_for/k)
  z <- week_for%%k
  start <- 1
  m <- (n-3)*625
  #start <- 625*z +1 #include more and more training data
  #m     <- (n*k-(1-z))*625 not accurate as long as data for the first 4 (?) matches are missing
  #m <- ((n-(2-z))*625) - 625
  
  
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
        name <- paste0("gw_",as.character(week_for))
        predictions_table <- regressors_test_data_16_k %>% mutate(
          index_int = as.integer(index)
        ) %>% select(index_int)
        
        predictions_table <- cbind(predictions_table,predictions)
        colnames(predictions_table)[j+1] <- name
        
      } else{
        
        # u for update:
        
        opponent_u <- opponent_round_16[week_for-5+j]
        team_u     <- team_round_16[week_for-5+j]
        
        # Create predictors
        regressors_test_data_16_k_u  <- regressors_16[(m+1):(m+625),]
        regressors_test_data_16_k_u[,"opponent"] <- opponent_u
        regressors_test_data_16_k_u[,"team"] <- team_u
        
        regressors_test_data_16_k_u <- regressors_test_data_16_k_u %>% na.omit()
        regressors_test_data_16_k_u  <- regressors_test_data_16_k_u %>% filter(index %in% regressors_train_16_k$index)
        regressors_test_16_k_u  <- regressors_test_data_16_k_u[,names(regressors_test_data_16_k) != "realized"]
        
        ##New predictions
        predictions_u <- predict(object = model_3,newdata = regressors_test_16_k_u)
        
        ##Store
        name <- paste0("gw_",as.character(week_for+j-1))
        predictions_table <- cbind(predictions_table,predictions_u)
        colnames(predictions_table)[j+1] <- name

      }
    }
  
  name <- paste0("forecasts_week_", as.character(week_for))
  assign(x = name,value = predictions_table)

}
