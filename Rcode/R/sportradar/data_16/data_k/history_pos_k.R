######################
# Distribute pos in k matches
######################

k <- 3

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




