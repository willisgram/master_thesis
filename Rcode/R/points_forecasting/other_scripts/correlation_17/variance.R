####################
# Script for finding the variance of each player
#####################
library(tidyverse)
library(xlsx)

# Points round n:
h <- 10

points_round_17_hold <- points_round_17[,names(points_round_17) != "index"]
points_round_n <- cbind(points_round_16,points_round_17_hold)
points_round_n_t <- t(points_round_n)


for (n in 1:29) {
  
  var_17 <- data.frame(index = 1:625,var=rep(0,625))

  for( i in 1:625){
    var_17$var[i] <- var(points_round_n_t[2:(39+n),i])
  }
  
  #var_17 <- var_17 %>% mutate(
   # std = sqrt(var)
  #)
  
  # Structure
  var_17[,3:(h+1)] <- var_17[,2]
  colnames(var_17)[2:(h+1)] <- paste0("round_",(n):(n+h-1))
  var_17[is.na(var_17)] <- mean(var_17[,2],na.rm = T)
  min_var <- min(var_17[var_17 > 0])
  var_17[var_17 == 0] <- min_var
  
  # Assign name
  name_var <- paste0("variance_GW", as.character(n),".xlsx")
  path_var <- '../../../input/dynamic_data/season_17/variance/'
  file_var <- paste0(path_var, name_var)
  
  # Write xlsx file
  rownames(var_17) <- NULL
  write.xlsx(var_17, file_var,row.names = F)

}






