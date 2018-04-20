##############
# Adjust data for first rounds 2016
############## 
library(tidyverse)
options(stringsAsFactors = F)
path <- '../../../Data/BK/GW1_4/'

for (i in 1:4) {
  
  name <- paste0(path,"GW",i,".csv")
  
  gw <- read.csv2(name)

  gw_name <- gw %>% mutate(
    Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
    FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
  )
  
  gw_all <- inner_join(players,gw_name,by = c("FirstName_1", "Surname_1"))
  
  if(i == 1){
    gw_points <- gw_all %>% select(index,PointsLastRound)  
  } else{
    gw_points_round <- gw_all %>% select(PointsLastRound)  
    gw_points <- cbind(gw_points,gw_points_round)
  }
  
}

points_round_16_edit <- points_round_16[,6:39]
index <- data.frame(index = 1:625)
points_round_16_temp <- merge(index,gw_points,by = "index",all = T)

points_round_16_edit <- cbind(points_round_16_temp,points_round_16_edit)
colnames(points_round_16_edit)[2:5] <- paste0("round_",1:4)
write.csv(x = points_round_16_edit,file = "load/data_16/data_16_output/points_round_16.csv",row.names = F)
