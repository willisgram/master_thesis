######################
# Distribute team in k matches
######################

k <- 3

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




