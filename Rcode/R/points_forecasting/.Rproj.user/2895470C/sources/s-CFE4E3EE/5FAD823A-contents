#######################
# Regression that handles 2016/2017 glide
#######################
# First realized is round 2

# Distribute points from k matches
######################
# Distribute points from k matches


options(stringsAsFactors = F)
k<-2 #not consider other time series aspect of points
upper_lim <- 1 #38-2*k+2 #+1 due to index, does not consider round 38

#KNOWN IN ADVANCE:

#Opponents
for (i in seq(from = 1,by = 1, to = upper_lim)) {
  
  opponent_round_temp <- data.frame(index = 1:625)
  opponent_round_temp[,2] <- opponent_round_17[,(i+k-1)]
  
  colnames(opponent_round_temp)[2] <- "opponent"
  
  if(i == 1){
    opponent_round_k_16_17 <- opponent_round_temp
  } else{
    opponent_round_k_16_17    <- rbind(opponent_round_k_16_17,opponent_round_temp)  
  }
  
}

#Team
for (i in seq(from = 1,by = 1, to = upper_lim)) {
  
  team_round_temp <- data.frame(index = 1:625)
  team_round_temp[,2] <- team_round_17[,(i+k-1)]
  
  colnames(team_round_temp)[2] <- "team"
  
  if(i == 1){
    team_round_k_16_17 <- team_round_temp
  } else{
    team_round_k_16_17    <- rbind(team_round_k_16_17,team_round_temp)  
  }
  
}

team_round_k_16_17 <- team_round_k_16_17 %>% select(team)

#Cost
for (i in seq(from = 1,by = 1, to = upper_lim)) {
  
  cost_round_temp <- data.frame(index = 1:625)
  cost_round_temp[,2] <- cost_round_17[,(i+k-1)]
  
  colnames(cost_round_temp)[2] <- "cost"
  
  if(i == 1){
    cost_round_k_16_17 <- cost_round_temp
  } else{
    cost_round_k_16_17    <- rbind(cost_round_k_16_17,cost_round_temp)  
  }
  
}

cost_round_k_16_17 <- cost_round_k_16_17 %>% select(cost)

#Pos
for (i in seq(from = 1,by = 1, to = upper_lim)) {
  
  pos_round_temp <- data.frame(index = 1:625)
  pos_round_temp[,2] <- pos_round_17[,((i+k-1))]
  
  colnames(pos_round_temp)[2] <- "pos"
  
  if(i == 1){
    pos_round_k_16_17 <- pos_round_temp
  } else{
    pos_round_k_16_17    <- rbind(pos_round_k_16_17,pos_round_temp)  
  }
  
}

pos_round_k_16_17 <- pos_round_k_16_17 %>% select(pos)

#Home/Away

h_a_17_reg <- h_a_17
h_a_17_reg[h_a_17_reg == "W"] <- NA
index <- data.frame(index = 1:625)
h_a_17_reg <- cbind(index,h_a_17_reg)

for (i in seq(from = 1,by = 1, to = upper_lim)) {
  
  h_a_17_reg_round_temp <- data.frame(index = 1:625)
  h_a_17_reg_round_temp[,2] <- h_a_17_reg[,((i+k-1))]
  
  colnames(h_a_17_reg_round_temp)[2] <- "H_A"
  
  if(i == 1){
    h_a_17_reg_round_k_16_17 <- h_a_17_reg_round_temp
  } else{
    h_a_17_reg_round_k_16_17    <- rbind(h_a_17_reg_round_k_16_17,h_a_17_reg_round_temp)  
  }
  
}

h_a_17_reg_round_k_16_17 <- h_a_17_reg_round_k_16_17 %>% select(H_A)

#KNOWN IN ADVANCE 5-37
#Total points last season, add if needed


regressors_16_17 <- cbind(opponent_round_k_16_17,team_round_k_16_17,cost_round_k_16_17,
                       pos_round_k_16_17,h_a_17_reg_round_k_16_17)

######################