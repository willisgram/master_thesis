######################
# Full 2016 data import
######################

#Points

###########
#Import training data
library(tidyverse)
library(xlsx)
options(stringsAsFactors = F)
folder <- "input/"
points_round_16 <- data.frame(index = 1:625) #See also dummies

for( i in 5:37){
  year     <- "16"
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
  
  points_round_16 <- cbind(points_round_16,data_temp$PointsLastRound)
  colnames(points_round_16)[i-3] <- paste0("round_",i)
  
}

#Get last round
data_37 <- read.csv("input/FPL16-GW37.csv")
data_37 <- data_37 %>% mutate(
  Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
  FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
)
data_37 <- full_join(data_37,players,by = c("FirstName_1" = "FirstName_1","Surname_1"="Surname_1"))
data_37 <- data_37[!is.na(data_37$index),]
data_37 <- data_37 %>% select(index,TotalPoints,PointsLastRound) %>% arrange(index)

#36
data_36 <- read.csv("input/FPL16-GW36.csv")
data_36 <- data_36 %>% mutate(
  Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
  FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
)
data_36 <- full_join(data_36,players,by = c("FirstName_1" = "FirstName_1","Surname_1"="Surname_1"))
data_36 <- data_36[!is.na(data_36$index),]
data_36 <- data_36 %>% select(index,TotalPoints) %>% arrange(index)

#Calculate
points_round_16 <- points_round_16 %>% mutate(
  round_37 = data_37$TotalPoints - data_36$TotalPoints - data_37$PointsLastRound,
  round_38 = data_37$PointsLastRound
)

write.csv(x = points_round_16,file = "data_16/data_16_output/points_round_16.csv",row.names = F)

#Opponents
opponent_round_16 <- data.frame(index = 1:625)

for( i in 5:37){
  year     <- "16"
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
  
  opponent_round_16 <- cbind(opponent_round_16,data_temp$NextFixture1)
  colnames(opponent_round_16)[i-3] <- paste0("round_",i+1)
  
}

opponent_round_16_1_4 <- opponent_round_16 %>% mutate(
  round_5 = round_23,
  round_4 = round_22,
  round_3 = round_21,
  round_2 = round_20,
  round_1 = round_19
) %>% select(round_1,round_2,round_3,round_4,round_5)

opponent_round_16 <- cbind(opponent_round_16_1_4,opponent_round_16)

opponent_round_16 <- opponent_round_16[,-6]
index = data.frame( index = 1:625)
opponent_round_16 <- cbind(index,opponent_round_16)

write.csv(x = opponent_round_16,file = "load/data_16/data_16_output/opponent_round_16.csv",row.names = F)

#Cost
cost_round_16 <- data.frame(index = 1:625)

for( i in 5:37){
  year     <- "16"
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
  
  cost_round_16 <- cbind(cost_round_16,data_temp$Cost)
  colnames(cost_round_16)[i-3] <- paste0("round_",i)
  
}

cost_round_16 <- cost_round_16 %>% mutate(
  round_38 = round_37
)

cost_round_16_1_4 <- cost_round_16 %>% mutate(
  round_1 = round_5,
  round_2 = round_5,
  round_3 = round_5,
  round_4 = round_5
) %>% select(round_1,round_2,round_3,round_4)

cost_round_16 <- cbind(cost_round_16_1_4,cost_round_16)

cost_round_16 <- cost_round_16[,-5]
index = data.frame( index = 1:625)
cost_round_16 <- cbind(index,cost_round_16)

write.csv(x = cost_round_16,file = "load/data_16/data_16_output/cost_round_16.csv",row.names = F)

# Structure and write xlsx
cost_round_16[is.na(cost_round_16)] <- 100000000
name_for <- paste0("player_cost.xlsx")
path_for <- '../../../input/static_data/cost/'
file_for <- paste0(path_for, name_for)

# Write xlsx file
rownames(cost_round_16) <- NULL
write.xlsx(cost_round_16, file_for,row.names = F)

#Team
team_round_16 <- data.frame(index = 1:625)

for( i in 5:37){
  year     <- "16"
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
  
  team_round_16 <- cbind(team_round_16,data_temp$Team.x)
  colnames(team_round_16)[i-3] <- paste0("round_",i)
  
}

team_round_16_1_4 <- team_round_16 %>% mutate(
  round_1 = round_5,
  round_2 = round_5,
  round_3 = round_5,
  round_4 = round_5
) %>% select(round_1,round_2,round_3,round_4)

team_round_16 <- cbind(team_round_16_1_4,team_round_16)

team_round_16_38 <- team_round_16 %>% mutate(
  round_38 = round_37
) %>% select(round_38)

team_round_16 <- cbind(team_round_16,team_round_16_38)

team_round_16 <- team_round_16[,-5]
index = data.frame( index = 1:625)
team_round_16 <- cbind(index,team_round_16)

write.csv(x = team_round_16,file = "load/data_16/data_16_output/team_round_16.csv",row.names = F)

#Position
pos_round_16 <- data.frame(index = 1:625)

for( i in 5:37){
  year     <- "16"
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
  
  pos_round_16 <- cbind(pos_round_16,data_temp$PositionsList.x)
  colnames(pos_round_16)[i-3] <- paste0("round_",i)
  
}

pos_round_16_1_4 <- pos_round_16 %>% mutate(
  round_1 = round_5,
  round_2 = round_5,
  round_3 = round_5,
  round_4 = round_5
) %>% select(round_1,round_2,round_3,round_4)

pos_round_16 <- cbind(pos_round_16_1_4,pos_round_16)

pos_round_16_38 <- pos_round_16 %>% mutate(
  round_38 = round_37
) %>% select(round_38)

pos_round_16 <- cbind(pos_round_16,pos_round_16_38)

pos_round_16 <- pos_round_16[,-5]
index = data.frame( index = 1:625)
pos_round_16 <- cbind(index,pos_round_16)

write.csv(x = pos_round_16,file = "load/data_16/data_16_output/pos_round_16.csv",row.names = F)


#Transfers in
trans_in_round_16 <- data.frame(index = 1:625)

for( i in 5:37){
  year     <- "16"
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
  
  trans_in_round_16 <- cbind(trans_in_round_16,data_temp$TransfersInRound)
  colnames(trans_in_round_16)[i-3] <- paste0("round_",i)
  
}

trans_in_round_16_1_4 <- trans_in_round_16 %>% mutate(
  round_1 = round(round_5/4),
  round_2 = round(round_5/4),
  round_3 = round(round_5/4),
  round_4 = round(round_5/4)
) %>% select(round_1,round_2,round_3,round_4)

trans_in_round_16 <- cbind(trans_in_round_16_1_4,trans_in_round_16)

trans_in_round_16_38 <- trans_in_round_16 %>% mutate(
  round_37 = round(rowMeans(trans_in_round_16[,2:36])),
  round_38 = round_37
) %>% select(round_37,round_38)

trans_in_round_16 <- subset(trans_in_round_16,select = -round_37)
trans_in_round_16 <- cbind(trans_in_round_16,trans_in_round_16_38)

trans_in_round_16 <- trans_in_round_16[,-5]
index = data.frame( index = 1:625)
trans_in_round_16 <- cbind(index,trans_in_round_16)

write.csv(x = trans_in_round_16,file = "load/data_16/data_16_output/trans_in_round_16.csv",row.names = F)


#Transfers out
trans_out_round_16 <- data.frame(index = 1:625)

for( i in 5:37){
  year     <- "16"
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
  
  trans_out_round_16 <- cbind(trans_out_round_16,data_temp$TransfersOutRound)
  colnames(trans_out_round_16)[i-3] <- paste0("round_",i)
  
}

trans_out_round_16_1_4 <- trans_out_round_16 %>% mutate(
  round_1 = round(round_5/4),
  round_2 = round(round_5/4),
  round_3 = round(round_5/4),
  round_4 = round(round_5/4)
) %>% select(round_1,round_2,round_3,round_4)

trans_out_round_16 <- cbind(trans_out_round_16_1_4,trans_out_round_16)

trans_out_round_16_38 <- trans_out_round_16 %>% mutate(
  round_37 = round(rowMeans(trans_out_round_16[,2:36])),
  round_38 = round_37
) %>% select(round_37,round_38)

trans_out_round_16 <- subset(trans_out_round_16,select = -round_37)
trans_out_round_16 <- cbind(trans_out_round_16,trans_out_round_16_38)

trans_out_round_16 <- trans_out_round_16[,-5]
index = data.frame( index = 1:625)
trans_out_round_16 <- cbind(index,trans_out_round_16)

write.csv(x = trans_out_round_16,file = "load/data_16/data_16_output/trans_out_round_16.csv",row.names = F)

# Total minutes played
tot_min_round_16 <- data.frame(index = 1:625)

for( i in 5:37){
  year     <- "16"
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
  
  tot_min_round_16 <- cbind(tot_min_round_16,data_temp$MinutesPlayed)
  colnames(tot_min_round_16)[i-3] <- paste0("round_",i)
  
}

#Minutes played, 90 > indicates double round

minutes_round_16 <- data.frame(index = 1:625)

for (i in 5:37) {
  
  if(i == 5){
    
    minutes_round_16[,i-3] <- tot_min_round_16[,i-3]
    
  } else{
  
    minutes_round_16[,i-3] <- if_else(condition = is.na(tot_min_round_16[,i-4])&!is.na(tot_min_round_16[,i-3]),
            true = tot_min_round_16[,i-3],
            false = tot_min_round_16[,i-3]-tot_min_round_16[,i-4])
    

  }
  
  colnames(minutes_round_16)[i-3] <- paste0("round_",i)
}

minutes_round_16_1_4 <- minutes_round_16 %>% mutate(
  round_temp = round_5,
  round_5 = round(round_temp/5),
  round_4 = round((round_temp-round_5)/4),
  round_3 = round((round_temp-round_5-round_4)/3),
  round_2 = round((round_temp-round_5-round_4-round_3)/2),
  round_1 = round((round_temp-round_5-round_4-round_3-round_2)/1)
) %>% select(round_1,round_2,round_3,round_4,round_5)

minutes_round_16 <- subset(minutes_round_16,select = -round_5)
minutes_round_16 <- cbind(minutes_round_16_1_4,minutes_round_16)

minutes_round_16_38 <- minutes_round_16 %>% mutate(
  round_temp = round_37,
  round_37 = round(round_temp/2),
  round_38 = round_temp-round_37
) %>% select(round_37,round_38)

minutes_round_16 <- subset(minutes_round_16,select = -round_37)
minutes_round_16 <- cbind(minutes_round_16,minutes_round_16_38)

minutes_round_16 <- minutes_round_16[,-6]
index = data.frame( index = 1:625)
minutes_round_16 <- cbind(index,minutes_round_16)

write.csv(x = minutes_round_16,file = "load/data_16/data_16_output/minutes_round_16.csv",row.names = F)


rm(list = c("data_temp","folder","sheet","week","year","path","i"))