######################
# Full 2017 data import for regression
######################


###########
#Import training data
library(tidyverse)
library(xlsx) 
options(stringsAsFactors = F)
folder <- "input/"
last_gw<-35

#Points
##########
points_round_17 <- data.frame(index = 1:625)

for( i in 1:last_gw){
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
##############

#Opponents
##############
opponent_round_17 <- data.frame(index = 1:625)

for( i in 0:(last_gw-1)){
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

opponent_round_17_short <- opponent_round_17
opponent_round_17_short[opponent_round_17_short == "Arsenal"] <- "ARS"
opponent_round_17_short[opponent_round_17_short == "Brighton"] <- "BHA"
opponent_round_17_short[opponent_round_17_short == "Bournemouth"] <- "BOU"
opponent_round_17_short[opponent_round_17_short == "Burnley"] <- "BUR"
opponent_round_17_short[opponent_round_17_short == "Chelsea"] <- "CHE"
opponent_round_17_short[opponent_round_17_short == "Crystal Palace"] <- "CRY"
opponent_round_17_short[opponent_round_17_short == "Everton"] <- "EVE"
opponent_round_17_short[opponent_round_17_short == "Huddersfield"] <- "HUD"
opponent_round_17_short[opponent_round_17_short == "Leicester"] <- "LEI"
opponent_round_17_short[opponent_round_17_short == "Liverpool"] <- "LIV"
opponent_round_17_short[opponent_round_17_short == "Man City"] <- "MCI"
opponent_round_17_short[opponent_round_17_short == "Man Utd"] <- "MUN"
opponent_round_17_short[opponent_round_17_short == "Newcastle"] <- "NEW"
opponent_round_17_short[opponent_round_17_short == "Southampton"] <- "SOU"
opponent_round_17_short[opponent_round_17_short == "Stoke"] <- "STK"
opponent_round_17_short[opponent_round_17_short == "Swansea"] <- "SWA"
opponent_round_17_short[opponent_round_17_short == "Tottenham"] <- "TOT"
opponent_round_17_short[opponent_round_17_short == "Watford"] <- "WAT"
opponent_round_17_short[opponent_round_17_short == "West Brom"] <- "WBA"
opponent_round_17_short[opponent_round_17_short == "West Ham"] <- "WHU"

write.csv(x = opponent_round_17_short,file = "load/data_17/data_17_output/opponent_round_17_short.csv",row.names = F)
#############

#Cost
##############
cost_round_17 <- data.frame(index = 1:625)

for( i in 0:(last_gw-1)){
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
###########

#Team
##############
team_round_17 <- data.frame(index = 1:625)

for( i in 0:(last_gw-1)){
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
  colnames(team_round_17)[i+2] <- paste0("round_",i+1)
  
}

#team_round_17[,31:39] <- team_round_17[,30]
#colnames(team_round_17)[31:39] <- paste0("round_",30:38) 

write.csv(x = team_round_17,file = "load/data_17/data_17_output/team_round_17.csv",row.names = F)
##################

#Position
############
pos_round_17 <- data.frame(index = 1:625)

for( i in 0:(last_gw-1)){
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

############

#Transfers in
#################
trans_in_round_17 <- data.frame(index = 1:625)

for( i in 1:last_gw){
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
################

#Transfers out
################
trans_out_round_17 <- data.frame(index = 1:625)

for( i in 1:last_gw){
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
#################

# Total minutes played
###############
tot_min_round_17 <- data.frame(index = 1:625)

for( i in 1:last_gw){
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
###############

#Minutes played, 90 > indicates double round
###############
minutes_round_17 <- data.frame(index = 1:625)

for (i in 1:last_gw) {
  
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
#################

#Total points
##########
tot_points_round_17 <- data.frame(index = 1:625)

for( i in 1:last_gw){
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
  
  data_temp <- data_temp %>% select(index,TotalPoints) %>% arrange(index)
  
  tot_points_round_17 <- cbind(tot_points_round_17,data_temp$TotalPoints)
  colnames(tot_points_round_17)[i+1] <- paste0("round_",i)
  
}

write.csv(x = tot_points_round_17,file = "load/data_17/data_17_output/tot_points_round_17.csv",row.names = F)
##############

#Points last season
##########
last_season_p_round_17 <- data.frame(index = 1:625)

for( i in 1:last_gw){
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
  
  data_temp <- data_temp %>% select(index,TotalPoints) %>% arrange(index)
  
  last_season_p_round_17 <- cbind(last_season_p_round_17,data_temp$TotalPoints)
  colnames(last_season_p_round_17)[i+1] <- paste0("round_",i)
  
}

write.csv(x = last_season_p_round_17,file = "load/data_17/data_17_output/last_season_p_round_17.csv",row.names = F)
##############

#Total Points last season
##########
last_season_tot_p_round_17 <- data.frame(index = 1:625)
last_season_tot_p_round_17[2:(last_gw+1)] <- last_season_p_round_17[,(last_gw+1)]
colnames(last_season_tot_p_round_17)[2:(last_gw+1)] <- paste0("round_",1:last_gw)
write.csv(x = last_season_p_round_17,file = "load/data_17/data_17_output/last_season_tot_p_round_17.csv",row.names = F)
###########


#Yellow cards round
##########
y_cards_round_17 <- data.frame(index = 1:625)

for( i in 1:last_gw){
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
  
  data_temp <- data_temp %>% select(index,YellowCards) %>% arrange(index)
  
  y_cards_round_17 <- cbind(y_cards_round_17,data_temp$YellowCards)
  colnames(y_cards_round_17)[i+1] <- paste0("round_",i)
  
}

write.csv(x = y_cards_round_17,file = "load/data_17/data_17_output/y_cards_round_17.csv",row.names = F)
##############

#Goals conceeded
##########
goals_con_round_17 <- data.frame(index = 1:625)

for( i in 1:last_gw){
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
  
  data_temp <- data_temp %>% select(index,GoalsConceded) %>% arrange(index)
  
  goals_con_round_17 <- cbind(goals_con_round_17,data_temp$GoalsConceded)
  colnames(goals_con_round_17)[i+1] <- paste0("round_",i)
  
}

write.csv(x = goals_con_round_17,file = "load/data_17/data_17_output/goals_con_round_17.csv",row.names = F)
##############

#Saves
##########
saves_round_17 <- data.frame(index = 1:625)

for( i in 1:last_gw){
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
  
  data_temp <- data_temp %>% select(index,Saves) %>% arrange(index)
  
  saves_round_17 <- cbind(saves_round_17,data_temp$Saves)
  colnames(saves_round_17)[i+1] <- paste0("round_",i)
  
}

write.csv(x = saves_round_17,file = "load/data_17/data_17_output/saves_round_17.csv",row.names = F)
##############

#Goals Scored
##########
goals_round_17 <- data.frame(index = 1:625)

for( i in 1:last_gw){
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
  
  data_temp <- data_temp %>% select(index,GoalsScored) %>% arrange(index)
  
  goals_round_17 <- cbind(goals_round_17,data_temp$GoalsScored)
  colnames(goals_round_17)[i+1] <- paste0("round_",i)
  
}

write.csv(x = goals_round_17,file = "load/data_17/data_17_output/goals_round_17.csv",row.names = F)
###########

#Pen miss
##########
pen_miss_round_17 <- data.frame(index = 1:625)

for( i in 1:last_gw){
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
  
  data_temp <- data_temp %>% select(index,PenaltiesMissed) %>% arrange(index)
  
  pen_miss_round_17 <- cbind(pen_miss_round_17,data_temp$PenaltiesMissed)
  colnames(pen_miss_round_17)[i+1] <- paste0("round_",i)
  
}

write.csv(x = pen_miss_round_17,file = "load/data_17/data_17_output/pen_miss_round_17.csv",row.names = F)
##############

#Pen save
##########
pen_save_round_17 <- data.frame(index = 1:625)

for( i in 1:last_gw){
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
  
  data_temp <- data_temp %>% select(index,PenaltiesSaved) %>% arrange(index)
  
  pen_save_round_17 <- cbind(pen_save_round_17,data_temp$PenaltiesSaved)
  colnames(pen_save_round_17)[i+1] <- paste0("round_",i)
  
}

write.csv(x = pen_save_round_17,file = "load/data_17/data_17_output/pen_save_round_17.csv",row.names = F)
##############

#Clean Sheet
##########
clean_sheet_round_17 <- data.frame(index = 1:625)

for( i in 1:last_gw){
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
  
  data_temp <- data_temp %>% select(index,CleanSheets) %>% arrange(index)
  
  clean_sheet_round_17 <- cbind(clean_sheet_round_17,data_temp$CleanSheets)
  colnames(clean_sheet_round_17)[i+1] <- paste0("round_",i)
  
}

write.csv(x = clean_sheet_round_17,file = "load/data_17/data_17_output/clean_sheet_round_17.csv",row.names = F)
##############

#Assists
##########
assists_round_17 <- data.frame(index = 1:625)

for( i in 1:last_gw){
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
  
  data_temp <- data_temp %>% select(index,Assists) %>% arrange(index)
  
  assists_round_17 <- cbind(assists_round_17,data_temp$Assists)
  colnames(assists_round_17)[i+1] <- paste0("round_",i)
  
}

write.csv(x = assists_round_17,file = "load/data_17/data_17_output/assists_round_17.csv",row.names = F)
##############

#Own goals
##########
own_goals_round_17 <- data.frame(index = 1:625)

for( i in 1:last_gw){
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
  
  data_temp <- data_temp %>% select(index,OwnGoals) %>% arrange(index)
  
  own_goals_round_17 <- cbind(own_goals_round_17,data_temp$OwnGoals)
  colnames(own_goals_round_17)[i+1] <- paste0("round_",i)
  
}

write.csv(x = own_goals_round_17,file = "load/data_17/data_17_output/own_goals_round_17.csv",row.names = F)
##############

#Dream Team
##########
dream_team_round_17 <- data.frame(index = 1:625)

for( i in 1:last_gw){
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
  
  data_temp <- data_temp %>% select(index,DreamteamCount) %>% arrange(index)
  
  dream_team_round_17 <- cbind(dream_team_round_17,data_temp$DreamteamCount)
  colnames(dream_team_round_17)[i+1] <- paste0("round_",i)
  
}

write.csv(x = dream_team_round_17,file = "load/data_17/data_17_output/dream_team_round_17.csv",row.names = F)
##############

#Red Cards
##########
r_cards_round_17 <- data.frame(index = 1:625)

for( i in 1:last_gw){
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
  
  data_temp <- data_temp %>% select(index,RedCards) %>% arrange(index)
  
  r_cards_round_17 <- cbind(r_cards_round_17,data_temp$RedCards)
  colnames(r_cards_round_17)[i+1] <- paste0("round_",i)
  
}

write.csv(x = r_cards_round_17,file = "load/data_17/data_17_output/r_cards_round_17.csv",row.names = F)
##############

#BPS
##########
BPS_round_17 <- data.frame(index = 1:625)

for( i in 1:last_gw){
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
  
  data_temp <- data_temp %>% select(index,BPS) %>% arrange(index)
  
  BPS_round_17 <- cbind(BPS_round_17,data_temp$BPS)
  colnames(BPS_round_17)[i+1] <- paste0("round_",i)
  
}

write.csv(x = BPS_round_17,file = "load/data_17/data_17_output/BPS_round_17.csv",row.names = F)
##############





