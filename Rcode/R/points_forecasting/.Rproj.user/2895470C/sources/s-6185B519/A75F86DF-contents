######################
# Full 2016 data import for new regressors, beginning in week 5
######################


###########
#Import training data
library(tidyverse)
library(xlsx) 
options(stringsAsFactors = F)
folder <- "input/"
#last_gw<-38

#Total points
##########
tot_points_round_16 <- data.frame(index = 1:625)

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
  
  data_temp <- data_temp %>% select(index,TotalPoints) %>% arrange(index)
  
  tot_points_round_16 <- cbind(tot_points_round_16,data_temp$TotalPoints)
  colnames(tot_points_round_16)[i-3] <- paste0("round_",i)
  
}

write.csv(x = tot_points_round_16,file = "load/data_16/data_16_output/tot_points_round_16.csv",row.names = F)
##############

#Points last season
##########
last_season_p_round_16 <- data.frame(index = 1:625)

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
  
  data_temp <- data_temp %>% select(index,TotalPoints) %>% arrange(index)
  
  last_season_p_round_16 <- cbind(last_season_p_round_16,data_temp$TotalPoints)
  colnames(last_season_p_round_16)[i-3] <- paste0("round_",i)
  
}

write.csv(x = last_season_p_round_16,file = "load/data_16/data_16_output/last_season_p_round_16.csv",row.names = F)
##############

#Total Points last season
##########
last_season_tot_p_round_16 <- data.frame(index = 1:625)
last_season_tot_p_round_16[2:(37)] <- last_season_p_round_16[,(34)]
colnames(last_season_tot_p_round_16)[2:34] <- paste0("round_",5:37)
write.csv(x = last_season_tot_p_round_16,file = "load/data_16/data_16_output/last_season_tot_p_round_16.csv",row.names = F)
###########

#Yellow cards round
##########
y_cards_round_16 <- data.frame(index = 1:625)

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
  
  data_temp <- data_temp %>% select(index,YellowCards) %>% arrange(index)
  
  y_cards_round_16 <- cbind(y_cards_round_16,data_temp$YellowCards)
  colnames(y_cards_round_16)[i-3] <- paste0("round_",i)
  
}

write.csv(x = y_cards_round_16,file = "load/data_16/data_16_output/y_cards_round_16.csv",row.names = F)
##############

#Goals conceeded
##########
goals_con_round_16 <- data.frame(index = 1:625)

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
  
  data_temp <- data_temp %>% select(index,GoalsConceded) %>% arrange(index)
  
  goals_con_round_16 <- cbind(goals_con_round_16,data_temp$GoalsConceded)
  colnames(goals_con_round_16)[i-3] <- paste0("round_",i)
  
}

write.csv(x = goals_con_round_16,file = "load/data_16/data_16_output/goals_con_round_16.csv",row.names = F)
##############

#Saves
##########
saves_round_16 <- data.frame(index = 1:625)

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
  
  data_temp <- data_temp %>% select(index,Saves) %>% arrange(index)
  
  saves_round_16 <- cbind(saves_round_16,data_temp$Saves)
  colnames(saves_round_16)[i-3] <- paste0("round_",i)
  
}

write.csv(x = saves_round_16,file = "load/data_16/data_16_output/saves_round_16.csv",row.names = F)
##############

#Goals Scored
##########
goals_round_16 <- data.frame(index = 1:625)

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
  
  data_temp <- data_temp %>% select(index,GoalsScored) %>% arrange(index)
  
  goals_round_16 <- cbind(goals_round_16,data_temp$GoalsScored)
  colnames(goals_round_16)[i-3] <- paste0("round_",i)
  
}

write.csv(x = goals_round_16,file = "load/data_16/data_16_output/goals_round_16.csv",row.names = F)
###########

#Pen miss
##########
pen_miss_round_16 <- data.frame(index = 1:625)

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
  
  data_temp <- data_temp %>% select(index,PenaltiesMissed) %>% arrange(index)
  
  pen_miss_round_16 <- cbind(pen_miss_round_16,data_temp$PenaltiesMissed)
  colnames(pen_miss_round_16)[i-3] <- paste0("round_",i)
  
}

write.csv(x = pen_miss_round_16,file = "load/data_16/data_16_output/pen_miss_round_16.csv",row.names = F)
##############

#Pen save
##########
pen_save_round_16 <- data.frame(index = 1:625)

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
  
  data_temp <- data_temp %>% select(index,PenaltiesMissed) %>% arrange(index)
  
  pen_save_round_16 <- cbind(pen_save_round_16,data_temp$PenaltiesMissed)
  colnames(pen_save_round_16)[i-3] <- paste0("round_",i)
  
}

write.csv(x = pen_save_round_16,file = "load/data_16/data_16_output/pen_save_round_16.csv",row.names = F)
##############

#Clean Sheet
##########
clean_sheet_round_16 <- data.frame(index = 1:625)

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
  
  data_temp <- data_temp %>% select(index,CleanSheets) %>% arrange(index)
  
  clean_sheet_round_16 <- cbind(clean_sheet_round_16,data_temp$CleanSheets)
  colnames(clean_sheet_round_16)[i-3] <- paste0("round_",i)
  
}

write.csv(x = clean_sheet_round_16,file = "load/data_16/data_16_output/clean_sheet_round_16.csv",row.names = F)
##############

#Assists
##########
assists_round_16 <- data.frame(index = 1:625)

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
  
  data_temp <- data_temp %>% select(index,Assists) %>% arrange(index)
  
  assists_round_16 <- cbind(assists_round_16,data_temp$Assists)
  colnames(assists_round_16)[i-3] <- paste0("round_",i)
  
}

write.csv(x = assists_round_16,file = "load/data_16/data_16_output/assists_round_16.csv",row.names = F)
##############

#Own goals
##########
own_goals_round_16 <- data.frame(index = 1:625)

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
  
  data_temp <- data_temp %>% select(index,OwnGoals) %>% arrange(index)
  
  own_goals_round_16 <- cbind(own_goals_round_16,data_temp$OwnGoals)
  colnames(own_goals_round_16)[i-3] <- paste0("round_",i)
  
}

write.csv(x = own_goals_round_16,file = "load/data_16/data_16_output/own_goals_round_16.csv",row.names = F)
##############

#Dream Team
##########
dream_team_round_16 <- data.frame(index = 1:625)

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
  
  data_temp <- data_temp %>% select(index,DreamteamCount) %>% arrange(index)
  
  dream_team_round_16 <- cbind(dream_team_round_16,data_temp$DreamteamCount)
  colnames(dream_team_round_16)[i-3] <- paste0("round_",i)
  
}

write.csv(x = dream_team_round_16,file = "load/data_16/data_16_output/dream_team_round_16.csv",row.names = F)
##############

#Red Cards
##########
r_cards_round_16 <- data.frame(index = 1:625)

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
  
  data_temp <- data_temp %>% select(index,RedCards) %>% arrange(index)
  
  r_cards_round_16 <- cbind(r_cards_round_16,data_temp$RedCards)
  colnames(r_cards_round_16)[i-3] <- paste0("round_",i)
  
}

write.csv(x = r_cards_round_16,file = "load/data_16/data_16_output/r_cards_round_16.csv",row.names = F)
##############

#BPS
##########
BPS_round_16 <- data.frame(index = 1:625)

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
  
  data_temp <- data_temp %>% select(index,BPS) %>% arrange(index)
  
  BPS_round_16 <- cbind(BPS_round_16,data_temp$BPS)
  colnames(BPS_round_16)[i-3] <- paste0("round_",i)
  
}

write.csv(x = BPS_round_16,file = "load/data_16/data_16_output/BPS_round_16.csv",row.names = F)
##############





