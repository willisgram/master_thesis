######################
# Distribute opponent in k matches
######################

k <- 3

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




