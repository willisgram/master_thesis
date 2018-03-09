######################
# Distribute points from k matches
######################

k <- 3
matches <- 27
upper_lim <- matches-(k-1)

#points
for (i in seq(from = 2,by = k, to = upper_lim)) {
  
  points_round_temp <- data.frame(index = 1:625)
  points_round_temp[,2:(k+1)] <- points_round_17[,i:(i+(k-1))]
  
  colnames(points_round_temp)[2:(k)] <- paste0("prev_",(k-1):1)
  colnames(points_round_temp)[(k+1)] <- "realized"
  
  if(i == 2){
    points_round_k_17 <- points_round_temp
  } else{
    points_round_k_17    <- rbind(points_round_k_17,points_round_temp)  
  }
  
}

#Opponents
for (i in seq(from = 2,by = k, to = upper_lim)) {
  
  opponent_round_temp <- data.frame(index = 1:625)
  opponent_round_temp[,2] <- opponent_round_17[,(i+(k-1))]
  
  colnames(opponent_round_temp)[2] <- "opponent"
  
  if(i == 2){
    opponent_round_k_17 <- opponent_round_temp
  } else{
    opponent_round_k_17    <- rbind(opponent_round_k_17,opponent_round_temp)  
  }
  
}

opponent_round_k_17 <- opponent_round_k_17 %>% select(opponent)

#Team
for (i in seq(from = 2,by = k, to = upper_lim)) {
  
  team_round_temp <- data.frame(index = 1:625)
  team_round_temp[,2] <- team_round_17[,(i)]
  
  colnames(team_round_temp)[2] <- "team"
  
  if(i == 2){
    team_round_k_17 <- team_round_temp
  } else{
    team_round_k_17    <- rbind(team_round_k_17,team_round_temp)  
  }
  
}

team_round_k_17 <- team_round_k_17 %>% select(team)

#Cost
for (i in seq(from = 2,by = k, to = upper_lim)) {
  
  cost_round_temp <- data.frame(index = 1:625)
  cost_round_temp[,2] <- cost_round_17[,(i)]
  
  colnames(cost_round_temp)[2] <- "cost"
  
  if(i == 2){
    cost_round_k_17 <- cost_round_temp
  } else{
    cost_round_k_17    <- rbind(cost_round_k_17,cost_round_temp)  
  }
  
}

cost_round_k_17 <- cost_round_k_17 %>% select(cost)

#Pos
for (i in seq(from = 2,by = k, to = upper_lim)) {
  
  pos_round_temp <- data.frame(index = 1:625)
  pos_round_temp[,2] <- pos_round_17[,(i)]
  
  colnames(pos_round_temp)[2] <- "pos"
  
  if(i == 2){
    pos_round_k_17 <- pos_round_temp
  } else{
    pos_round_k_17    <- rbind(pos_round_k_17,pos_round_temp)  
  }
  
}

pos_round_k_17 <- pos_round_k_17 %>% select(pos)

#Transfers in
for (i in seq(from = 2,by = k, to = upper_lim)) {
  
  trans_in_round_temp <- data.frame(index = 1:625)
  trans_in_round_temp[,2:(k+1)] <- trans_in_round_17[,i:(i+(k-1))]
  
  colnames(trans_in_round_temp)[2:(k)] <- paste0("trans_in_prev_",(k-1):1)
  colnames(trans_in_round_temp)[(k+1)] <- "realized"
  
  if(i == 2){
    trans_in_round_k_17 <- trans_in_round_temp
  } else{
    trans_in_round_k_17    <- rbind(trans_in_round_k_17,trans_in_round_temp)  
  }
  
}

trans_in_round_k_17 <- trans_in_round_k_17 %>% select(trans_in_prev_2,trans_in_prev_1)

#Transfers out
for (i in seq(from = 2,by = k, to = upper_lim)) {
  
  trans_out_round_temp <- data.frame(index = 1:625)
  trans_out_round_temp[,2:(k+1)] <- trans_out_round_17[,i:(i+(k-1))]
  
  colnames(trans_out_round_temp)[2:(k)] <- paste0("trans_out_prev_",(k-1):1)
  colnames(trans_out_round_temp)[(k+1)] <- "realized"
  
  if(i == 2){
    trans_out_round_k_17 <- trans_out_round_temp
  } else{
    trans_out_round_k_17 <- rbind(trans_out_round_k_17,trans_out_round_temp)  
  }
  
}

trans_out_round_k_17 <- trans_out_round_k_17 %>% select(trans_out_prev_2,trans_out_prev_1)






























