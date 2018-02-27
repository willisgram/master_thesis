######################
# Distribute points from k matches
######################

week_for <- 16
k <- 3
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































