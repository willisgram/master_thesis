######################
# Distribute points from k matches
######################

k <- 2

#points
for (i in seq(from = 2,by = k, to = 34)) {
  
  points_round_temp <- data.frame(index = 1:625)
  points_round_temp[,2:(k+1)] <- points_round[,i:(i+(k-1))]
  
  colnames(points_round_temp)[2:(k)] <- paste0("prev_",(k-1):1)
  colnames(points_round_temp)[(k+1)] <- "realized"
  
  if(i == 2){
    points_round_k <- points_round_temp
  } else{
    points_round_k    <- rbind(points_round_k,points_round_temp)  
  }
  
}

#Opponents
for (i in seq(from = 2,by = k, to = 34)) {
  
  opponent_round_temp <- data.frame(index = 1:625)
  opponent_round_temp[,2] <- opponent_round[,(i)]
  
  colnames(opponent_round_temp)[2] <- "opponent"
  
  if(i == 2){
    opponent_round_k <- opponent_round_temp
  } else{
    opponent_round_k    <- rbind(opponent_round_k,opponent_round_temp)  
  }
  
}

opponent_round_k <- opponent_round_k %>% select(opponent)

#Team
for (i in seq(from = 2,by = k, to = 34)) {
  
  team_round_temp <- data.frame(index = 1:625)
  team_round_temp[,2] <- team_round[,(i)]
  
  colnames(team_round_temp)[2] <- "team"
  
  if(i == 2){
    team_round_k <- team_round_temp
  } else{
    team_round_k    <- rbind(team_round_k,team_round_temp)  
  }
  
}

team_round_k <- team_round_k %>% select(team)

#Cost
for (i in seq(from = 2,by = k, to = 34)) {
  
  cost_round_temp <- data.frame(index = 1:625)
  cost_round_temp[,2] <- cost_round[,(i)]
  
  colnames(cost_round_temp)[2] <- "cost"
  
  if(i == 2){
    cost_round_k <- cost_round_temp
  } else{
    cost_round_k    <- rbind(cost_round_k,cost_round_temp)  
  }
  
}

cost_round_k <- cost_round_k %>% select(cost)

#Pos
for (i in seq(from = 2,by = k, to = 34)) {
  
  pos_round_temp <- data.frame(index = 1:625)
  pos_round_temp[,2] <- pos_round[,(i)]
  
  colnames(pos_round_temp)[2] <- "pos"
  
  if(i == 2){
    pos_round_k <- pos_round_temp
  } else{
    pos_round_k    <- rbind(pos_round_k,pos_round_temp)  
  }
  
}

pos_round_k <- pos_round_k %>% select(pos)


#################
# Create regressors DF
#################

regressors <- cbind(points_round_k,opponent_round_k,team_round_k,pos_round_k,cost_round_k)
regressors$index <- as.factor(regressors$index)


regressors_train <- regressors[1:6250,] %>% na.omit()
regressors_test_data  <- regressors[6251:6875,] %>% na.omit()
regressors_test_data  <- regressors_test_data %>% filter(index %in% regressors_train$index)
regressors_test  <- regressors_test_data[,names(regressors_test_data) != "realized"]





























