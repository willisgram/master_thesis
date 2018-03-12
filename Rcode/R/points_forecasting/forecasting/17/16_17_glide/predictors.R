#################
# Predictors
#################


k <- 6 # That is, 5 previous are needed
h <- 5 # Horizon of forecast

## CONSTANT IN FORECAST
# Points:
for (i in 1:10) {
  
  if(i < k){
    if(i == 1){
      prev_points <- points_round_16[,(40+(i)-k):(39)]
    } else{
      prev_points_16 <- points_round_16[,(40+(i)-k):(39)]
      prev_points_17 <- points_round_17[,2:(i)]  
      prev_points <- cbind(prev_points_16,prev_points_17)
    }
    
  } else{
    prev_points <- points_round_17[,(i-k+2):(i)]  
  }
  
  colnames(prev_points)[1:(k-1)] <- paste0("prev_",(k-1):1)
  
}

#Cost
  cost_round <- cost_round_17[,(i+1)]
  colnames(cost_round_temp)[1] <- "cost"
  

#Pos
  pos_round <- pos_round_17[,(i+1)]
  colnames(pos_round_temp)[1] <- "pos"
  

#trans_in
  if(i == 1){
    trans_in_round <- trans_in_round_16[,(39)]
    colnames(trans_in_round_temp)[1] <- "trans_in"
  } else{
    trans_in_round <- trans_in_round_17[,(i+1)]
    colnames(trans_in_round_temp)[1] <- "trans_in" 
  }
  
#trans_out
  if(i == 1){
    trans_out_round <- trans_out_round_16[,(39)]
    colnames(trans_out_round_temp)[1] <- "trans_out"
  } else{
    trans_out_round <- trans_out_round_17[,(i+1)]
    colnames(trans_out_round_temp)[1] <- "trans_out" 
  }  


## UPDATED IN FORECAST
#Opponents
  opponent_round <- opponent_round_17[,(i+1)]
  colnames(opponent_round_temp)[1] <- "opponent"
  
#Team
  team_round <- team_round_17[,(i+1)]
  colnames(team_round_temp)[1] <- "team"
  
  for (j in 1:h) {
    
    if(j == 1){
      #Create regressors
      index <- data.frame(index = 1:625)
      predictors <- cbind(index, points_round,opponent_round,team_round,cost_round,
                             pos_round,trans_in_round,trans_out_round)
      predictors$index <- as.factor(predictors$index)
      
      options(stringsAsFactors = T)
      
      ##Predict
      predictions <- predict(object = model_k,newdata = predictors)
      
      
      ##Store
      name <- paste0("GW_",as.character(i))
      predictions_table <- regressors_test_data_16_k %>% mutate(
        index_int = as.integer(index)
      ) %>% select(index_int)
      
      predictions_table <- cbind(predictions_table,predictions)
      colnames(predictions_table)[j+1] <- name
      
      
    } else{
      
      # u for update:
      
      opponent_u <- opponent_round_17[i+j+1]
      team_u     <- team_round_16[i+j+1]
      
      # Create predictors
      #regressors_test_data_16_k_u  <- regressors_16[(m+1):(m+625),]
      predictors[,"opponent"] <- opponent_u
      predictors[,"team"] <- team_u
      
      regressors_test_data_16_k_u <- regressors_test_data_16_k_u %>% na.omit()
      regressors_test_data_16_k_u  <- regressors_test_data_16_k_u %>% filter(index %in% regressors_train_16_k$index)
      regressors_test_16_k_u  <- regressors_test_data_16_k_u[,names(regressors_test_data_16_k) != "realized"]
      
      ##New predictions
      predictions_u <- predict(object = model_k,newdata = predictors_u)
      
      ##Store
      name <- paste0("gw_",as.character(i+j-1))
      predictions_table <- cbind(predictions_table,predictions_u)
      colnames(predictions_table)[j+1] <- name
      
    }
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  

