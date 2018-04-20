##############
# Adjust data for first rounds 2016
############## 
library(tidyverse)
options(stringsAsFactors = F)
path <- '../../../Data/BK/GW1_4/'

#Points

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



#Minutes played

for (i in 1:4) {
  
  name <- paste0(path,"GW",i,".csv")
  
  gw <- read.csv2(name)
  
  gw_name <- gw %>% mutate(
    Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
    FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
  )
  
  gw_all <- inner_join(players,gw_name,by = c("FirstName_1", "Surname_1"))
  
  if(i == 1){
    gw_minutes <- gw_all %>% select(index,MinutesPlayed)  
  } else{
    gw_minutes_round <- gw_all %>% select(MinutesPlayed)  
    gw_minutes <- cbind(gw_minutes,gw_minutes_round)
  }
  
}

minutes_round_16_edit <- minutes_round_16[,6:39]
index <- data.frame(index = 1:625)
minutes_round_16_temp <- merge(index,gw_minutes,by = "index",all = T)

minutes_round_16_edit <- cbind(minutes_round_16_temp,minutes_round_16_edit)
colnames(minutes_round_16_edit)[2:5] <- paste0("round_",1:4)
write.csv(x = minutes_round_16_edit,file = "load/data_16/data_16_output/minutes_round_16.csv",row.names = F)

#opponent

i<-1

name <- paste0(path,"GW",i,".csv")

gw <- read.csv2(name)

gw_name <- gw %>% mutate(
  Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
  FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
)

gw_all <- inner_join(players,gw_name,by = c("FirstName_1", "Surname_1"))
gw_opponent <- gw_all %>% select(index,NextFixture1,NextFixture2,NextFixture3,NextFixture4)


opponent_round_16_edit <- opponent_round_16[,7:39]
index <- data.frame(index = 1:625)
opponent_round_16_temp <- merge(index,gw_opponent,by = "index",all = T)

opponent_round_1 <-full_join(players,opponents_16,"Team") %>% select(Gw1)
opponent_round_1 <- opponent_round_1$Gw1[1:625]
opponent_round_16_temp <- cbind(opponent_round_1,opponent_round_16_temp[2:5])
opponent_round_16_temp <- cbind(index,opponent_round_16_temp)

opponent_round_16_edit <- cbind(opponent_round_16_temp,opponent_round_16_edit)
colnames(opponent_round_16_edit)[2:6] <- paste0("round_",1:5)
write.csv(x = opponent_round_16_edit,file = "load/data_16/data_16_output/opponent_round_16.csv",row.names = F)


opponent_round_16_short <- opponent_round_16
opponent_round_16_short[opponent_round_16_short == "Arsenal"] <- "ARS"
opponent_round_16_short[opponent_round_16_short == "Brighton"] <- "BHA"
opponent_round_16_short[opponent_round_16_short == "Bournemouth"] <- "BOU"
opponent_round_16_short[opponent_round_16_short == "Burnley"] <- "BUR"
opponent_round_16_short[opponent_round_16_short == "Chelsea"] <- "CHE"
opponent_round_16_short[opponent_round_16_short == "Crystal Palace"] <- "CRY"
opponent_round_16_short[opponent_round_16_short == "Everton"] <- "EVE"
opponent_round_16_short[opponent_round_16_short == "Huddersfield"] <- "HUD"
opponent_round_16_short[opponent_round_16_short == "Leicester"] <- "LEI"
opponent_round_16_short[opponent_round_16_short == "Liverpool"] <- "LIV"
opponent_round_16_short[opponent_round_16_short == "Man City"] <- "MCI"
opponent_round_16_short[opponent_round_16_short == "Man Utd"] <- "MUN"
opponent_round_16_short[opponent_round_16_short == "Newcastle"] <- "NEW"
opponent_round_16_short[opponent_round_16_short == "Southampton"] <- "SOU"
opponent_round_16_short[opponent_round_16_short == "Stoke"] <- "STK"
opponent_round_16_short[opponent_round_16_short == "Swansea"] <- "SWA"
opponent_round_16_short[opponent_round_16_short == "Tottenham"] <- "TOT"
opponent_round_16_short[opponent_round_16_short == "Watford"] <- "WAT"
opponent_round_16_short[opponent_round_16_short == "West Brom"] <- "WBA"
opponent_round_16_short[opponent_round_16_short == "West Ham"] <- "WHU"

write.csv(x = opponent_round_16_short,file = "load/data_16/data_16_output/opponent_round_16_short.csv",row.names = F)













