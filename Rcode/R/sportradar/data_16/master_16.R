#####################
# Master loop to finish all prediction sheets
#####################
library(xlsx)
# index - gw_1 - gw_2 ...

# Suggestion, only change opponent and team
# Not using forecast as new previous



#given input
k <- 3
h <- 5

for(week_for in 15:25){
  
  # Distribute points from k matches
  ######################
  # Distribute points from k matches
  
  
  options(stringsAsFactors = F)
  lower_lim <- week_for%%k+4
  upper_lim <- 34-(k-1)
  
  #points
  for (i in seq(from = lower_lim,by = k, to = upper_lim)) {
    
    points_round_temp <- data.frame(index = 1:625)
    points_round_temp[,2:(k+1)] <- points_round_16[,i:(i+(k-1))]
    
    colnames(points_round_temp)[2:(k)] <- paste0("prev_",(k-1):1)
    colnames(points_round_temp)[(k+1)] <- "realized"
    
    if(i == lower_lim){
      points_round_k_16 <- points_round_temp
    } else{
      points_round_k_16 <- rbind(points_round_k_16,points_round_temp)  
    }
    
  }
  
  #Opponents
  for (i in seq(from = lower_lim,by = k, to = upper_lim)) {
    
    opponent_round_temp <- data.frame(index = 1:625)
    opponent_round_temp[,2] <- opponent_round_16[,(i+1)]
    
    colnames(opponent_round_temp)[2] <- "opponent"
    
    if(i == lower_lim){
      opponent_round_k_16 <- opponent_round_temp
    } else{
      opponent_round_k_16    <- rbind(opponent_round_k_16,opponent_round_temp)  
    }
    
  }
  
  opponent_round_k_16 <- opponent_round_k_16 %>% select(opponent)
  
  #Team
  for (i in seq(from = lower_lim,by = k, to = upper_lim)) {
    
    team_round_temp <- data.frame(index = 1:625)
    team_round_temp[,2] <- team_round_16[,(i)]
    
    colnames(team_round_temp)[2] <- "team"
    
    if(i == lower_lim){
      team_round_k_16 <- team_round_temp
    } else{
      team_round_k_16    <- rbind(team_round_k_16,team_round_temp)  
    }
    
  }
  
  team_round_k_16 <- team_round_k_16 %>% select(team)
  
  #Cost
  for (i in seq(from = lower_lim,by = k, to = upper_lim)) {
    
    cost_round_temp <- data.frame(index = 1:625)
    cost_round_temp[,2] <- cost_round_16[,(i)]
    
    colnames(cost_round_temp)[2] <- "cost"
    
    if(i == lower_lim){
      cost_round_k_16 <- cost_round_temp
    } else{
      cost_round_k_16    <- rbind(cost_round_k_16,cost_round_temp)  
    }
    
  }
  
  cost_round_k_16 <- cost_round_k_16 %>% select(cost)
  
  #Pos
  for (i in seq(from = lower_lim,by = k, to = upper_lim)) {
    
    pos_round_temp <- data.frame(index = 1:625)
    pos_round_temp[,2] <- pos_round_16[,(i)]
    
    colnames(pos_round_temp)[2] <- "pos"
    
    if(i == lower_lim){
      pos_round_k_16 <- pos_round_temp
    } else{
      pos_round_k_16    <- rbind(pos_round_k_16,pos_round_temp)  
    }
    
  }
  
  pos_round_k_16 <- pos_round_k_16 %>% select(pos)
  
  #Transfers in
  for (i in seq(from = lower_lim,by = k, to = upper_lim)) {
    
    trans_in_round_temp <- data.frame(index = 1:625)
    trans_in_round_temp[,2:(k+1)] <- trans_in_round_16[,i:(i+(k-1))]
    
    colnames(trans_in_round_temp)[2:(k)] <- paste0("trans_in_prev_",(k-1):1)
    colnames(trans_in_round_temp)[(k+1)] <- "realized"
    
    if(i == lower_lim){
      trans_in_round_k_16 <- trans_in_round_temp
    } else{
      trans_in_round_k_16    <- rbind(trans_in_round_k_16,trans_in_round_temp)  
    }
    
  }
  
  trans_in_round_k_16 <- trans_in_round_k_16 %>% select(trans_in_prev_2,trans_in_prev_1)
  
  #Transfers out
  for (i in seq(from = lower_lim,by = k, to = upper_lim)) {
    
    trans_out_round_temp <- data.frame(index = 1:625)
    trans_out_round_temp[,2:(k+1)] <- trans_out_round_16[,i:(i+(k-1))]
    
    colnames(trans_out_round_temp)[2:(k)] <- paste0("trans_out_prev_",(k-1):1)
    colnames(trans_out_round_temp)[(k+1)] <- "realized"
    
    if(i == lower_lim){
      trans_out_round_k_16 <- trans_out_round_temp
    } else{
      trans_out_round_k_16    <- rbind(trans_out_round_k_16,trans_out_round_temp)  
    }
    
  }
  
  trans_out_round_k_16 <- trans_out_round_k_16 %>% select(trans_out_prev_2,trans_out_prev_1)
  ######################
  
  # Create regressors
  ##################
  #Create regressors
  regressors_16 <- cbind(points_round_k_16,opponent_round_k_16,team_round_k_16,cost_round_k_16,
                         pos_round_k_16,trans_in_round_k_16,trans_out_round_k_16)
  regressors_16$index <- as.factor(regressors_16$index)
  #################
  
  # Generate forecasts
  ###########
  #Generate forecasts
  
  # index-keepers
  n <- floor(week_for/k)
  z <- week_for%%k
  start <- 1
  m <- (n-3)*625
  
  
  #Forecasts begins in week 6
  
  for (j in 1:5) {
    
    
    
    if(j == 1){
      #Create regressors
      regressors_train_16_k <- regressors_16[start:m,] %>% na.omit()
      regressors_test_data_16_k  <- regressors_16[(m+1):(m+625),] %>% na.omit()
      regressors_test_data_16_k  <- regressors_test_data_16_k %>% filter(index %in% regressors_train_16_k$index)
      regressors_test_16_k  <- regressors_test_data_16_k[,names(regressors_test_data_16_k) != "realized"]
      
      options(stringsAsFactors = T)
      
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
      
      cost_table <- regressors_test_data_16_k %>% mutate(
        index_int = as.integer(index)
      ) %>% select(index_int,cost)
      
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
  #################
  
  # Stucture back to standard format and write xlsx
  ###############
  
  # Create NAs for predictions
  names(predictions_table)[names(predictions_table) == "index_int"] <- "index"
  index_temp <- data.frame(index = 1:625)
  predictions_table <- full_join(predictions_table, index_temp, by = "index") %>% arrange(index)
  predictions_table[is.na(predictions_table)] <- -10000
  
  # Create NAs for cost
  names(cost_table)[names(cost_table) == "index_int"] <- "index"
  index_temp <- data.frame(index = 1:625)
  cost_table <- full_join(cost_table, index_temp, by = "index") %>% arrange(index)
  cost_table[is.na(predictions_table)] <- -10000
  cost_table[,3:5] <- cost_table[,2]
  
  # Assign name
  name <- paste0("forecast_point_gw", as.character(week_for),".xlsx")
  path <- '../../../input/dynamic_data/forecasting_method/regression/'
  file <- paste0(path, name)
  
  assign(x = name,value = predictions_table)
  
  # Write xlsx file
  rownames(predictions_table) <- NULL
  write.xlsx(predictions_table, file,row.names = F)
  #################
}
