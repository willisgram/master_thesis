#################
# Pred = average from a preveious games
#################


################
# Create dataframe
################
a <- 3
points_average_a_17 <- data.frame(index = 1:625)

#points_average_a_17[,2:5] <- NA

for(i in 1:29){
  
  if(i <= a){
    if(i == 1){
      points_average_a_17[,i+1] <- rowMeans(points_round_16[,(39+(i)-a):(39)])
    } else if(i == 2){
      points_average_a_17[,i+1] <- (rowSums(points_round_16[,(39+(i)-a):(39)]) + points_round_17[,2])/a
    } else if(i == a){
      points_average_a_17[,i+1] <- (points_round_16[,39] + rowSums(points_round_17[,2:(i)]))/a
    } else{
      points_average_a_17[,i+1] <- (rowSums(points_round_16[,(39+(i)-a):(39)]) + rowSums(points_round_17[,2:(i)]))/a
    }
    
    
  } else{
    
    points_average_a_17[,i+1] <- rowMeans(points_round_17[,((i+1)-a):(i)])
    
  }    
  
}
colnames(points_average_a_17)[2:30] <- paste0("round_",1:29)


## Adjust for home/away

home <- 1.129
away <- 0.871

# Create dummy dataframe
h_a <- data.frame(index = 1:625)
h_a[,2:30] = "a"
colnames(h_a)[2:30] <- paste0("round_",1:29)
h_a[1:300,seq(from = 2,by = 2,to = 29)] = "h"
h_a[301:625,seq(from = 3,by = 2,to = 30)] = "h"



h_a_2 <- h_a
h_a_2[h_a_2=="h"] <- as.numeric(home)
h_a_2[h_a_2=="a"] <- as.numeric(away)

h_a_2[,2:30] = as.numeric(h_a_2[,2:30])
h_a_3 <- data.matrix(h_a_2)

score <- points_average_a_17[,2:30]*h_a_3[,2:30]








