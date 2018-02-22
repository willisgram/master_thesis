######################
# Distribute cost in k matches
######################

k <- 3

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
