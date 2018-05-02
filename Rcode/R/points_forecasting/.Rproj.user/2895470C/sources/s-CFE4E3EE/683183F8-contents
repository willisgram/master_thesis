#######################
# Regression for GLK that does not take into account time series of points
#######################
# First realized is round 2

# Distribute points from k matches
######################
# Distribute points from k matches


options(stringsAsFactors = F)
#2 <- week_for%%k+4
k<-2 #not consider time series aspect of points
upper_lim <- 35 #38-2*k+2 #+1 due to index, does not consider round 38

#KNOWN IN ADVANCE:

#Opponents
for (i in seq(from = 2,by = 1, to = upper_lim)) {
  
  opponent_round_temp <- data.frame(index = 1:625)
  opponent_round_temp[,2] <- opponent_round_17[,(i+k-1)]
  
  colnames(opponent_round_temp)[2] <- "opponent"
  
  if(i == 2){
    opponent_round_k_17 <- opponent_round_temp
  } else{
    opponent_round_k_17    <- rbind(opponent_round_k_17,opponent_round_temp)  
  }
  
}

opponent_round_k_17 <- opponent_round_k_17 %>% select(opponent)
opponent_round_k_17$opponent <- if_else(
  opponent_round_k_17$opponent == "Hull City",true = "Huddersfield",false = opponent_round_k_17$opponent)
opponent_round_k_17$opponent <- if_else(
  opponent_round_k_17$opponent == "Middlesbrough",true = "Brighton",false = opponent_round_k_17$opponent) 
opponent_round_k_17$opponent <- if_else(
  opponent_round_k_17$opponent == "Sunderland",true = "Newcastle",false = opponent_round_k_17$opponent) 

#Team
for (i in seq(from = 2,by = 1, to = upper_lim)) {
  
  team_round_temp <- data.frame(index = 1:625)
  team_round_temp[,2] <- team_round_17[,(i+k-1)]
  
  colnames(team_round_temp)[2] <- "team"
  
  if(i == 2){
    team_round_k_17 <- team_round_temp
  } else{
    team_round_k_17    <- rbind(team_round_k_17,team_round_temp)  
  }
  
}

team_round_k_17 <- team_round_k_17 %>% select(team)

#Cost
for (i in seq(from = 2,by = 1, to = upper_lim)) {
  
  cost_round_temp <- data.frame(index = 1:625)
  cost_round_temp[,2] <- cost_round_17[,(i+k-1)]
  
  colnames(cost_round_temp)[2] <- "cost"
  
  if(i == 2){
    cost_round_k_17 <- cost_round_temp
  } else{
    cost_round_k_17    <- rbind(cost_round_k_17,cost_round_temp)  
  }
  
}

cost_round_k_17 <- cost_round_k_17 %>% select(cost)

#Pos
for (i in seq(from = 2,by = 1, to = upper_lim)) {
  
  pos_round_temp <- data.frame(index = 1:625)
  pos_round_temp[,2] <- pos_round_17[,((i+k-1))]
  
  colnames(pos_round_temp)[2] <- "pos"
  
  if(i == 2){
    pos_round_k_17 <- pos_round_temp
  } else{
    pos_round_k_17    <- rbind(pos_round_k_17,pos_round_temp)  
  }
  
}

pos_round_k_17 <- pos_round_k_17 %>% select(pos)

#Home/Away

h_a_17_reg <- h_a_17
h_a_17_reg[h_a_17_reg == "W"] <- NA
index <- data.frame(index = 1:625)
h_a_17_reg <- cbind(index,h_a_17_reg)

for (i in seq(from = 2,by = 1, to = upper_lim)) {
  
  h_a_17_reg_round_temp <- data.frame(index = 1:625)
  h_a_17_reg_round_temp[,2] <- h_a_17_reg[,((i+k-1))]
  
  colnames(h_a_17_reg_round_temp)[2] <- "H_A"
  
  if(i == 2){
    h_a_17_reg_round_k_17 <- h_a_17_reg_round_temp
  } else{
    h_a_17_reg_round_k_17    <- rbind(h_a_17_reg_round_k_17,h_a_17_reg_round_temp)  
  }
  
}

h_a_17_reg_round_k_17 <- h_a_17_reg_round_k_17 %>% select(H_A)

#KNOWN IN ADVANCE 5-37
#Total points last season

for (i in seq(from = 2,by = 1, to = upper_lim)) {
  
  last_season_tot_p_round_temp <- data.frame(index = 1:625)
  last_season_tot_p_round_temp[,2] <- last_season_tot_p_round_17[,i-2+k]
  
  colnames(last_season_tot_p_round_temp)[2] <- "last_season_tot_p" 
  
  if(i == 2){
    last_season_tot_p_round_k_17 <- last_season_tot_p_round_temp
  } else{
    last_season_tot_p_round_k_17    <- rbind(last_season_tot_p_round_k_17,last_season_tot_p_round_temp)  
  }
  
}

last_season_tot_p_round_k_17 <- last_season_tot_p_round_k_17 %>% select(last_season_tot_p)


#ONLY KNOWN FOR PREVIOUS

#points
for (i in seq(from = 2,by = 1, to = upper_lim)) {
  
  points_round_temp <- data.frame(index = 1:625)
  points_round_temp[,2:(k+1)] <- points_round_17[,i:(i+(k-1))]
  
  colnames(points_round_temp)[2:(k)] <- paste0("prev_",(k-1):1)
  colnames(points_round_temp)[(k+1)] <- "realized"
  
  if(i == 2){
    points_round_k_17 <- points_round_temp
  } else{
    points_round_k_17 <- rbind(points_round_k_17,points_round_temp)  
  }
  
}

#Transfers in
for (i in seq(from = 2,by = 1, to = upper_lim)) {
  
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

trans_in_round_k_17 <- trans_in_round_k_17 %>% select(2:k)

#Transfers out
for (i in seq(from = 2,by = 1, to = upper_lim)) {
  
  trans_out_round_temp <- data.frame(index = 1:625)
  trans_out_round_temp[,2:(k+1)] <- trans_out_round_17[,i:(i+(k-1))]
  
  colnames(trans_out_round_temp)[2:(k)] <- paste0("trans_out_prev_",(k-1):1)
  colnames(trans_out_round_temp)[(k+1)] <- "realized"
  
  if(i == 2){
    trans_out_round_k_17 <- trans_out_round_temp
  } else{
    trans_out_round_k_17    <- rbind(trans_out_round_k_17,trans_out_round_temp)  
  }
  
}

trans_out_round_k_17 <- trans_out_round_k_17 %>% select(2:k)

#Minutes Played
for (i in seq(from = 2,by = 1, to = upper_lim)) {
  
  minutes_round_temp <- data.frame(index = 1:625)
  minutes_round_temp[,2] <- minutes_round_17[,i-2+k]
  
  colnames(minutes_round_temp)[2] <- "minutes" 
  
  if(i == 2){
    minutes_round_k_17 <- minutes_round_temp
  } else{
    minutes_round_k_17    <- rbind(minutes_round_k_17,minutes_round_temp)  
  }
  
}

minutes_round_k_17 <- minutes_round_k_17 %>% select(minutes)

#only data from round 5
#Total Points
for (i in seq(from = 2,by = 1, to = upper_lim)) {
  
  tot_points_round_temp <- data.frame(index = 1:625)
  tot_points_round_temp[,2] <- tot_points_round_17[,i-2+k]
  
  colnames(tot_points_round_temp)[2] <- "tot_points" 
  
  if(i == 2){
    tot_points_round_k_17 <- tot_points_round_temp
  } else{
    tot_points_round_k_17    <- rbind(tot_points_round_k_17,tot_points_round_temp)  
  }
  
}

tot_points_round_k_17 <- tot_points_round_k_17 %>% select(tot_points)

#Yellow cards
for (i in seq(from = 2,by = 1, to = upper_lim)) {
  
  y_cards_round_temp <- data.frame(index = 1:625)
  y_cards_round_temp[,2] <- y_cards_round_17[,i-2+k]
  
  colnames(y_cards_round_temp)[2] <- "y_cards" 
  
  if(i == 2){
    y_cards_round_k_17 <- y_cards_round_temp
  } else{
    y_cards_round_k_17    <- rbind(y_cards_round_k_17,y_cards_round_temp)  
  }
  
}

y_cards_round_k_17 <- y_cards_round_k_17 %>% select(y_cards)

#Goals Conceeded
for (i in seq(from = 2,by = 1, to = upper_lim)) {
  
  goals_con_round_temp <- data.frame(index = 1:625)
  goals_con_round_temp[,2] <- goals_con_round_17[,i-2+k]
  
  colnames(goals_con_round_temp)[2] <- "goals_con" 
  
  if(i == 2){
    goals_con_round_k_17 <- goals_con_round_temp
  } else{
    goals_con_round_k_17    <- rbind(goals_con_round_k_17,goals_con_round_temp)  
  }
  
}

goals_con_round_k_17 <- goals_con_round_k_17 %>% select(goals_con)

#Saves
for (i in seq(from = 2,by = 1, to = upper_lim)) {
  
  saves_round_temp <- data.frame(index = 1:625)
  saves_round_temp[,2] <- saves_round_17[,i-2+k]
  
  colnames(saves_round_temp)[2] <- "saves" 
  
  if(i == 2){
    saves_round_k_17 <- saves_round_temp
  } else{
    saves_round_k_17    <- rbind(saves_round_k_17,saves_round_temp)  
  }
  
}

saves_round_k_17 <- saves_round_k_17 %>% select(saves)

#Goals
for (i in seq(from = 2,by = 1, to = upper_lim)) {
  
  goals_round_temp <- data.frame(index = 1:625)
  goals_round_temp[,2] <- goals_round_17[,i-2+k]
  
  colnames(goals_round_temp)[2] <- "goals" 
  
  if(i == 2){
    goals_round_k_17 <- goals_round_temp
  } else{
    goals_round_k_17    <- rbind(goals_round_k_17,goals_round_temp)  
  }
  
}

goals_round_k_17 <- goals_round_k_17 %>% select(goals)

#Penalty miss
for (i in seq(from = 2,by = 1, to = upper_lim)) {
  
  pen_miss_round_temp <- data.frame(index = 1:625)
  pen_miss_round_temp[,2] <- pen_miss_round_17[,i-2+k]
  
  colnames(pen_miss_round_temp)[2] <- "pen_miss" 
  
  if(i == 2){
    pen_miss_round_k_17 <- pen_miss_round_temp
  } else{
    pen_miss_round_k_17    <- rbind(pen_miss_round_k_17,pen_miss_round_temp)  
  }
  
}

pen_miss_round_k_17 <- pen_miss_round_k_17 %>% select(pen_miss)

#Penalty save
for (i in seq(from = 2,by = 1, to = upper_lim)) {
  
  pen_save_round_temp <- data.frame(index = 1:625)
  pen_save_round_temp[,2] <- pen_save_round_17[,i-2+k]
  
  colnames(pen_save_round_temp)[2] <- "pen_save" 
  
  if(i == 2){
    pen_save_round_k_17 <- pen_save_round_temp
  } else{
    pen_save_round_k_17    <- rbind(pen_save_round_k_17,pen_save_round_temp)  
  }
  
}

pen_save_round_k_17 <- pen_save_round_k_17 %>% select(pen_save)

#Clean sheets
for (i in seq(from = 2,by = 1, to = upper_lim)) {
  
  clean_sheet_round_temp <- data.frame(index = 1:625)
  clean_sheet_round_temp[,2] <- clean_sheet_round_17[,i-2+k]
  
  colnames(clean_sheet_round_temp)[2] <- "clean_sheet" 
  
  if(i == 2){
    clean_sheet_round_k_17 <- clean_sheet_round_temp
  } else{
    clean_sheet_round_k_17    <- rbind(clean_sheet_round_k_17,clean_sheet_round_temp)  
  }
  
}

clean_sheet_round_k_17 <- clean_sheet_round_k_17 %>% select(clean_sheet)

#Assists
for (i in seq(from = 2,by = 1, to = upper_lim)) {
  
  assists_round_temp <- data.frame(index = 1:625)
  assists_round_temp[,2] <- assists_round_17[,i-2+k]
  
  colnames(assists_round_temp)[2] <- "assists" 
  
  if(i == 2){
    assists_round_k_17 <- assists_round_temp
  } else{
    assists_round_k_17    <- rbind(assists_round_k_17,assists_round_temp)  
  }
  
}

assists_round_k_17 <- assists_round_k_17 %>% select(assists)

#Own Goals
for (i in seq(from = 2,by = 1, to = upper_lim)) {
  
  own_goals_round_temp <- data.frame(index = 1:625)
  own_goals_round_temp[,2] <- own_goals_round_17[,i-2+k]
  
  colnames(own_goals_round_temp)[2] <- "own_goals" 
  
  if(i == 2){
    own_goals_round_k_17 <- own_goals_round_temp
  } else{
    own_goals_round_k_17    <- rbind(own_goals_round_k_17,own_goals_round_temp)  
  }
  
}

own_goals_round_k_17 <- own_goals_round_k_17 %>% select(own_goals)

#Dream Team
for (i in seq(from = 2,by = 1, to = upper_lim)) {
  
  dream_team_round_temp <- data.frame(index = 1:625)
  dream_team_round_temp[,2] <- dream_team_round_17[,i-2+k]
  
  colnames(dream_team_round_temp)[2] <- "dream_team" 
  
  if(i == 2){
    dream_team_round_k_17 <- dream_team_round_temp
  } else{
    dream_team_round_k_17    <- rbind(dream_team_round_k_17,dream_team_round_temp)  
  }
  
}

dream_team_round_k_17 <- dream_team_round_k_17 %>% select(dream_team)

#Red cards
for (i in seq(from = 2,by = 1, to = upper_lim)) {
  
  r_cards_round_temp <- data.frame(index = 1:625)
  r_cards_round_temp[,2] <- r_cards_round_17[,i-2+k]
  
  colnames(r_cards_round_temp)[2] <- "r_cards" 
  
  if(i == 2){
    r_cards_round_k_17 <- r_cards_round_temp
  } else{
    r_cards_round_k_17    <- rbind(r_cards_round_k_17,r_cards_round_temp)  
  }
  
}

r_cards_round_k_17 <- r_cards_round_k_17 %>% select(r_cards)

#BPS
for (i in seq(from = 2,by = 1, to = upper_lim)) {
  
  BPS_round_temp <- data.frame(index = 1:625)
  BPS_round_temp[,2] <- BPS_round_17[,i-2+k]
  
  colnames(BPS_round_temp)[2] <- "BPS" 
  
  if(i == 2){
    BPS_round_k_17 <- BPS_round_temp
  } else{
    BPS_round_k_17    <- rbind(BPS_round_k_17,BPS_round_temp)  
  }
  
}

BPS_round_k_17 <- BPS_round_k_17 %>% select(BPS)


regressors_17 <- cbind(points_round_k_17,opponent_round_k_17,team_round_k_17,cost_round_k_17,
                       pos_round_k_17,trans_in_round_k_17,trans_out_round_k_17,last_season_tot_p_round_k_17,h_a_17_reg_round_k_17,minutes_round_k_17,
                       tot_points_round_k_17, y_cards_round_k_17, goals_con_round_k_17, saves_round_k_17, goals_round_k_17, pen_miss_round_k_17,
                       pen_save_round_k_17, clean_sheet_round_k_17,assists_round_k_17, own_goals_round_k_17, dream_team_round_k_17,
                       r_cards_round_k_17, BPS_round_k_17)

######################