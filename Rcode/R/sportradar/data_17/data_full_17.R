######################
# Full 2017 data import
######################

#Points

###########
#Import training data
library(tidyverse)
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

#Opponents
opponent_round_17 <- data.frame(index = 1:625)

for( i in 0:28){
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

#Cost
cost_round_17 <- data.frame(index = 1:625)

for( i in 0:28){
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

#Team
team_round_17 <- data.frame(index = 1:625)

for( i in 0:28){
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

#Position
pos_round_17 <- data.frame(index = 1:625)

for( i in 0:28){
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

#Transfers in
trans_in_round_17 <- data.frame(index = 1:625)

for( i in 1:28){
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

#Transfers out
trans_out_round_17 <- data.frame(index = 1:625)

for( i in 1:28){
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
