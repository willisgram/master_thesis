######################
# Distribute points from k matches
######################

k <- 3

for (i in seq(from = 2,by = k, to = 34)) {
  
  points_round_temp <- data.frame(index = 1:625)
  points_round_temp[,2:(k+1)] <- points_round[,i:(i+2)]
  
  colnames(points_round_temp)[2:(k)] <- paste0("prev_",(k-1):1)
  colnames(points_round_temp)[(k+1)] <- "realized"
  
  if(i == 2){
    points_round_k <- points_round_temp
  } else{
    points_round_k    <- rbind(points_round_k,points_round_temp)  
  }
  
}




