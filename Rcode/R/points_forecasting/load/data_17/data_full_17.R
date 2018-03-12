######################
# Full 2017 data import
######################

#Points

###########
#Import training data
library(tidyverse)
library(xlsx) #cost
options(stringsAsFactors = F)
folder <- "input/"
points_round_17 <- data.frame(index = 1:625)

for( i in 1:27){
  year     <- "17"
  week   <- as.character(i)
  sheet  <- paste0("FPL",year,"-GW",week,".csv")
  path   <- paste0(folder,sheet)
  
  
  data_temp   <- read.csv(path)
  
  
  data_temp <- data_temp %>% mutate(
    Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
    FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
  )
  
  
  data_temp <- full_join(data_temp,players,by = c("FirstName_1" = "FirstName_1","Surname_1"="Surname_1"))
  data_temp <- data_temp[!is.na(data_temp$index),]
  
  data_temp <- data_temp %>% select(index,PointsLastRound) %>% arrange(index)
  
  points_round_17 <- cbind(points_round_17,data_temp$PointsLastRound)
  colnames(points_round_17)[i+1] <- paste0("round_",i)
  
}

write.csv(x = points_round_17,file = "load/data_17/data_17_output/points_round_17.csv",row.names = F)


#Opponents
opponent_round_17 <- data.frame(index = 1:625)

for( i in 0:26){
  year     <- "17"
  week   <- as.character(i)
  sheet  <- paste0("FPL",year,"-GW",week,".csv")
  path   <- paste0(folder,sheet)
  
  
  data_temp   <- read.csv(path)
  
  
  data_temp <- data_temp %>% mutate(
    Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
    FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
  )
  
  
  data_temp <- full_join(data_temp,players,by = c("FirstName_1" = "FirstName_1","Surname_1"="Surname_1"))
  data_temp <- data_temp[!is.na(data_temp$index),]
  
  data_temp <- data_temp %>% select(index,NextFixture1) %>% arrange(index)
  
  opponent_round_17 <- cbind(opponent_round_17,data_temp$NextFixture1)
  colnames(opponent_round_17)[i+2] <- paste0("round_",i+1)
  
}

write.csv(x = opponent_round_17,file = "load/data_17/data_17_output/opponent_round_17.csv",row.names = F)

#Cost
cost_round_17 <- data.frame(index = 1:625)

for( i in 0:26){
  year     <- "17"
  week   <- as.character(i)
  sheet  <- paste0("FPL",year,"-GW",week,".csv")
  path   <- paste0(folder,sheet)
  
  
  data_temp   <- read.csv(path)
  
  
  data_temp <- data_temp %>% mutate(
    Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
    FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
  )
  
  
  data_temp <- full_join(data_temp,players,by = c("FirstName_1" = "FirstName_1","Surname_1"="Surname_1"))
  data_temp <- data_temp[!is.na(data_temp$index),]
  
  data_temp <- data_temp %>% select(index,Cost) %>% arrange(index)
  
  cost_round_17 <- cbind(cost_round_17,data_temp$Cost)
  colnames(cost_round_17)[i+2] <- paste0("round_",i+1)
  
}

write.csv(x = cost_round_17,file = "load/data_17/data_17_output/cost_round_17.csv",row.names = F)

# Structure and write xlsx
cost_round_17[is.na(cost_round_17)] <- 100000000
name_for <- paste0("player_cost.xlsx")
path_for <- '../../../input/static_data/cost/'
file_for <- paste0(path_for, name_for)

# Write xlsx file
rownames(cost_round_17) <- NULL
write.xlsx(cost_round_17, file_for,row.names = F)

#Team
team_round_17 <- data.frame(index = 1:625)

for( i in 0:26){
  year     <- "17"
  week   <- as.character(i)
  sheet  <- paste0("FPL",year,"-GW",week,".csv")
  path   <- paste0(folder,sheet)
  
  
  data_temp   <- read.csv(path)
  
  
  data_temp <- data_temp %>% mutate(
    Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
    FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
  )
  
  
  data_temp <- full_join(data_temp,players,by = c("FirstName_1" = "FirstName_1","Surname_1"="Surname_1"))
  data_temp <- data_temp[!is.na(data_temp$index),]
  
  data_temp <- data_temp %>% select(index,Team.x) %>% arrange(index)
  
  team_round_17 <- cbind(team_round_17,data_temp$Team.x)
  colnames(team_round_17)[i+1] <- paste0("round_",i)
  
}

write.csv(x = team_round_17,file = "load/data_17/data_17_output/team_round_17.csv",row.names = F)

#Position
pos_round_17 <- data.frame(index = 1:625)

for( i in 0:26){
  year     <- "17"
  week   <- as.character(i)
  sheet  <- paste0("FPL",year,"-GW",week,".csv")
  path   <- paste0(folder,sheet)
  
  
  data_temp   <- read.csv(path)
  
  
  data_temp <- data_temp %>% mutate(
    Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
    FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
  )
  
  
  data_temp <- full_join(data_temp,players,by = c("FirstName_1" = "FirstName_1","Surname_1"="Surname_1"))
  data_temp <- data_temp[!is.na(data_temp$index),]
  
  data_temp <- data_temp %>% select(index,PositionsList.x) %>% arrange(index)
  
  pos_round_17 <- cbind(pos_round_17,data_temp$PositionsList.x)
  colnames(pos_round_17)[i+2] <- paste0("round_",i+1)
  
}

write.csv(x = pos_round_17,file = "load/data_17/data_17_output/pos_round_17.csv",row.names = F)

#Transfers in
trans_in_round_17 <- data.frame(index = 1:625)

for( i in 1:27){
  year     <- "17"
  week   <- as.character(i)
  sheet  <- paste0("FPL",year,"-GW",week,".csv")
  path   <- paste0(folder,sheet)
  
  
  data_temp   <- read.csv(path)
  
  
  data_temp <- data_temp %>% mutate(
    Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
    FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
  )
  
  
  data_temp <- full_join(data_temp,players,by = c("FirstName_1" = "FirstName_1","Surname_1"="Surname_1"))
  data_temp <- data_temp[!is.na(data_temp$index),]
  
  data_temp <- data_temp %>% select(index,TransfersInRound) %>% arrange(index)
  
  trans_in_round_17 <- cbind(trans_in_round_17,data_temp$TransfersInRound)
  colnames(trans_in_round_17)[i+1] <- paste0("round_",i)
  
}

write.csv(x = trans_in_round_17,file = "load/data_17/data_17_output/trans_in_round_17.csv",row.names = F)

#Transfers out
trans_out_round_17 <- data.frame(index = 1:625)

for( i in 1:27){
  year     <- "17"
  week   <- as.character(i)
  sheet  <- paste0("FPL",year,"-GW",week,".csv")
  path   <- paste0(folder,sheet)
  
  
  data_temp   <- read.csv(path)
  
  
  data_temp <- data_temp %>% mutate(
    Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
    FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
  )
  
  
  data_temp <- full_join(data_temp,players,by = c("FirstName_1" = "FirstName_1","Surname_1"="Surname_1"))
  data_temp <- data_temp[!is.na(data_temp$index),]
  
  data_temp <- data_temp %>% select(index,TransfersOutRound) %>% arrange(index)
  
  trans_out_round_17 <- cbind(trans_out_round_17,data_temp$TransfersOutRound)
  colnames(trans_out_round_17)[i+1] <- paste0("round_",i)
  
}

write.csv(x = trans_out_round_17,file = "load/data_17/data_17_output/trans_out_round_17.csv",row.names = F)

# Total minutes played
tot_min_round_17 <- data.frame(index = 1:625)

for( i in 1:29){
  year     <- "17"
  week   <- as.character(i)
  sheet  <- paste0("FPL",year,"-GW",week,".csv")
  path   <- paste0(folder,sheet)
  
  
  data_temp   <- read.csv(path)
  
  
  data_temp <- data_temp %>% mutate(
    Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
    FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
  )
  
  
  data_temp <- full_join(data_temp,players,by = c("FirstName_1" = "FirstName_1","Surname_1"="Surname_1"))
  data_temp <- data_temp[!is.na(data_temp$index),]
  
  data_temp <- data_temp %>% select(index,MinutesPlayed) %>% arrange(index)
  
  tot_min_round_17 <- cbind(tot_min_round_17,data_temp$MinutesPlayed)
  colnames(tot_min_round_17)[i+1] <- paste0("round_",i)
  
}

#Minutes played, 90 > indicates double round

minutes_round_17 <- data.frame(index = 1:625)

for (i in 1:27) {
  
  if(i == 1){
    
    minutes_round_17[,i+1] <- tot_min_round_17[,i+1]
    
  } else{
    
    minutes_round_17[,i+1] <- if_else(condition = is.na(tot_min_round_17[,i])&!is.na(tot_min_round_17[,i+1]),
                                      true = tot_min_round_17[,i+1],
                                      false = tot_min_round_17[,i+1]-tot_min_round_17[,i])
    
    
  }
  
  colnames(minutes_round_17)[i+1] <- paste0("round_",i)
}

write.csv(x = minutes_round_17,file = "load/data_17/data_17_output/minutes_round_17.csv",row.names = F)






