#######################
# Regression for GLK that does not take into account time series of points
#######################
# First realized is round 6

# Distribute points from k matches
######################
# Distribute points from k matches


options(stringsAsFactors = F)
#2 <- week_for%%k+4
k<-2 #not consider time series aspect of points
upper_lim <- 38-2*k+2 #+1 due to index, does not consider round 38

#KNOWN IN ADVANCE:

#Opponents
for (i in seq(from = 6,by = 1, to = upper_lim)) {
  
  opponent_round_temp <- data.frame(index = 1:625)
  opponent_round_temp[,2] <- opponent_round_16[,(i+k-1)]
  
  colnames(opponent_round_temp)[2] <- "opponent"
  
  if(i == 6){
    opponent_round_k_16 <- opponent_round_temp
  } else{
    opponent_round_k_16    <- rbind(opponent_round_k_16,opponent_round_temp)  
  }
  
}

opponent_round_k_16 <- opponent_round_k_16 %>% select(opponent)
opponent_round_k_16$opponent <- if_else(
  opponent_round_k_16$opponent == "Hull City",true = "Huddersfield",false = opponent_round_k_16$opponent)
opponent_round_k_16$opponent <- if_else(
  opponent_round_k_16$opponent == "Middlesbrough",true = "Brighton",false = opponent_round_k_16$opponent) 
opponent_round_k_16$opponent <- if_else(
  opponent_round_k_16$opponent == "Sunderland",true = "Newcastle",false = opponent_round_k_16$opponent) 

#Team
for (i in seq(from = 6,by = 1, to = upper_lim)) {
  
  team_round_temp <- data.frame(index = 1:625)
  team_round_temp[,2] <- team_round_16[,(i+k-1)]
  
  colnames(team_round_temp)[2] <- "team"
  
  if(i == 6){
    team_round_k_16 <- team_round_temp
  } else{
    team_round_k_16    <- rbind(team_round_k_16,team_round_temp)  
  }
  
}

team_round_k_16 <- team_round_k_16 %>% select(team)

#Cost
for (i in seq(from = 6,by = 1, to = upper_lim)) {
  
  cost_round_temp <- data.frame(index = 1:625)
  cost_round_temp[,2] <- cost_round_16[,(i+k-1)]
  
  colnames(cost_round_temp)[2] <- "cost"
  
  if(i == 6){
    cost_round_k_16 <- cost_round_temp
  } else{
    cost_round_k_16    <- rbind(cost_round_k_16,cost_round_temp)  
  }
  
}

cost_round_k_16 <- cost_round_k_16 %>% select(cost)

#Pos
for (i in seq(from = 6,by = 1, to = upper_lim)) {
  
  pos_round_temp <- data.frame(index = 1:625)
  pos_round_temp[,2] <- pos_round_16[,((i+k-1))]
  
  colnames(pos_round_temp)[2] <- "pos"
  
  if(i == 6){
    pos_round_k_16 <- pos_round_temp
  } else{
    pos_round_k_16    <- rbind(pos_round_k_16,pos_round_temp)  
  }
  
}

pos_round_k_16 <- pos_round_k_16 %>% select(pos)

#ONLY KNOWN FOR PREVIOUS

#Transfers in
for (i in seq(from = 6,by = 1, to = upper_lim)) {
  
  trans_in_round_temp <- data.frame(index = 1:625)
  trans_in_round_temp[,2:(k+1)] <- trans_in_round_16[,i:(i+(k-1))]
  
  colnames(trans_in_round_temp)[2:(k)] <- paste0("trans_in_prev_",(k-1):1)
  colnames(trans_in_round_temp)[(k+1)] <- "realized"
  
  if(i == 6){
    trans_in_round_k_16 <- trans_in_round_temp
  } else{
    trans_in_round_k_16    <- rbind(trans_in_round_k_16,trans_in_round_temp)  
  }
  
}

trans_in_round_k_16 <- trans_in_round_k_16 %>% select(2:k)

#Transfers out
for (i in seq(from = 6,by = 1, to = upper_lim)) {
  
  trans_out_round_temp <- data.frame(index = 1:625)
  trans_out_round_temp[,2:(k+1)] <- trans_out_round_16[,i:(i+(k-1))]
  
  colnames(trans_out_round_temp)[2:(k)] <- paste0("trans_out_prev_",(k-1):1)
  colnames(trans_out_round_temp)[(k+1)] <- "realized"
  
  if(i == 6){
    trans_out_round_k_16 <- trans_out_round_temp
  } else{
    trans_out_round_k_16    <- rbind(trans_out_round_k_16,trans_out_round_temp)  
  }
  
}

trans_out_round_k_16 <- trans_out_round_k_16 %>% select(2:k)

#only data from round 5
#Yellow cards
for (i in seq(from = 6,by = 1, to = upper_lim)) {
  
  y_cards_round_temp <- data.frame(index = 1:625)
  y_cards_round_temp[,2] <- y_cards_round_16[,i-4]
  
  colnames(y_cards_round_temp)[2] <- "y_cards" 
  
  if(i == 6){
    y_cards_round_k_16 <- y_cards_round_temp
  } else{
    y_cards_round_k_16    <- rbind(y_cards_round_k_16,y_cards_round_temp)  
  }
  
}

y_cards_round_k_16 <- y_cards_round_k_16 %>% select(y_cards)

#Goals Conceeded
for (i in seq(from = 6,by = 1, to = upper_lim)) {
  
  goals_con_round_temp <- data.frame(index = 1:625)
  goals_con_round_temp[,2] <- goals_con_round_16[,i-4]
  
  colnames(goals_con_round_temp)[2] <- "goals_con" 
  
  if(i == 6){
    goals_con_round_k_16 <- goals_con_round_temp
  } else{
    goals_con_round_k_16    <- rbind(goals_con_round_k_16,goals_con_round_temp)  
  }
  
}

goals_con_round_k_16 <- goals_con_round_k_16 %>% select(goals_con)

#Saves
for (i in seq(from = 6,by = 1, to = upper_lim)) {
  
  saves_round_temp <- data.frame(index = 1:625)
  saves_round_temp[,2] <- saves_round_16[,i-4]
  
  colnames(saves_round_temp)[2] <- "saves" 
  
  if(i == 6){
    saves_round_k_16 <- saves_round_temp
  } else{
    saves_round_k_16    <- rbind(saves_round_k_16,saves_round_temp)  
  }
  
}

saves_round_k_16 <- saves_round_k_16 %>% select(saves)

#Goals
for (i in seq(from = 6,by = 1, to = upper_lim)) {
  
  goals_round_temp <- data.frame(index = 1:625)
  goals_round_temp[,2] <- goals_round_16[,i-4]
  
  colnames(goals_round_temp)[2] <- "goals" 
  
  if(i == 6){
    goals_round_k_16 <- goals_round_temp
  } else{
    goals_round_k_16    <- rbind(goals_round_k_16,goals_round_temp)  
  }
  
}

goals_round_k_16 <- goals_round_k_16 %>% select(goals)

#Penalty miss
for (i in seq(from = 6,by = 1, to = upper_lim)) {
  
  pen_miss_round_temp <- data.frame(index = 1:625)
  pen_miss_round_temp[,2] <- pen_miss_round_16[,i-4]
  
  colnames(pen_miss_round_temp)[2] <- "pen_miss" 
  
  if(i == 6){
    pen_miss_round_k_16 <- pen_miss_round_temp
  } else{
    pen_miss_round_k_16    <- rbind(pen_miss_round_k_16,pen_miss_round_temp)  
  }
  
}

pen_miss_round_k_16 <- pen_miss_round_k_16 %>% select(pen_miss)

#Clean sheets
for (i in seq(from = 6,by = 1, to = upper_lim)) {
  
  clean_sheet_round_temp <- data.frame(index = 1:625)
  clean_sheet_round_temp[,2] <- clean_sheet_round_16[,i-4]
  
  colnames(clean_sheet_round_temp)[2] <- "clean_sheet" 
  
  if(i == 6){
    clean_sheet_round_k_16 <- clean_sheet_round_temp
  } else{
    clean_sheet_round_k_16    <- rbind(clean_sheet_round_k_16,clean_sheet_round_temp)  
  }
  
}

clean_sheet_round_k_16 <- clean_sheet_round_k_16 %>% select(clean_sheet)

#Assists
for (i in seq(from = 6,by = 1, to = upper_lim)) {
  
  assits_round_temp <- data.frame(index = 1:625)
  assits_round_temp[,2] <- assits_round_16[,i-4]
  
  colnames(assits_round_temp)[2] <- "assits" 
  
  if(i == 6){
    assits_round_k_16 <- assits_round_temp
  } else{
    assits_round_k_16    <- rbind(assits_round_k_16,assits_round_temp)  
  }
  
}

assits_round_k_16 <- assits_round_k_16 %>% select(assits)

#Own Goals
for (i in seq(from = 6,by = 1, to = upper_lim)) {
  
  own_goals_round_temp <- data.frame(index = 1:625)
  own_goals_round_temp[,2] <- own_goals_round_16[,i-4]
  
  colnames(own_goals_round_temp)[2] <- "own_goals" 
  
  if(i == 6){
    own_goals_round_k_16 <- own_goals_round_temp
  } else{
    own_goals_round_k_16    <- rbind(own_goals_round_k_16,own_goals_round_temp)  
  }
  
}

own_goals_round_k_16 <- own_goals_round_k_16 %>% select(own_goals)

#Dream Team
for (i in seq(from = 6,by = 1, to = upper_lim)) {
  
  dream_team_round_temp <- data.frame(index = 1:625)
  dream_team_round_temp[,2] <- dream_team_round_16[,i-4]
  
  colnames(dream_team_round_temp)[2] <- "dream_team" 
  
  if(i == 6){
    dream_team_round_k_16 <- dream_team_round_temp
  } else{
    dream_team_round_k_16    <- rbind(dream_team_round_k_16,dream_team_round_temp)  
  }
  
}

dream_team_round_k_16 <- dream_team_round_k_16 %>% select(dream_team)

#Red cards
for (i in seq(from = 6,by = 1, to = upper_lim)) {
  
  r_cards_round_temp <- data.frame(index = 1:625)
  r_cards_round_temp[,2] <- r_cards_round_16[,i-4]
  
  colnames(r_cards_round_temp)[2] <- "r_cards" 
  
  if(i == 6){
    r_cards_round_k_16 <- r_cards_round_temp
  } else{
    r_cards_round_k_16    <- rbind(r_cards_round_k_16,r_cards_round_temp)  
  }
  
}

r_cards_round_k_16 <- r_cards_round_k_16 %>% select(r_cards)

#BPS
for (i in seq(from = 6,by = 1, to = upper_lim)) {
  
  BPS_round_temp <- data.frame(index = 1:625)
  BPS_round_temp[,2] <- BPS_round_16[,i-4]
  
  colnames(BPS_round_temp)[2] <- "BPS" 
  
  if(i == 6){
    BPS_round_k_16 <- BPS_round_temp
  } else{
    BPS_round_k_16    <- rbind(BPS_round_k_16,BPS_round_temp)  
  }
  
}

BPS_round_k_16 <- BPS_round_k_16 %>% select(BPS)

######################