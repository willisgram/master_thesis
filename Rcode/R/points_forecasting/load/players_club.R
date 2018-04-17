################
#Complete dynamic player list
################
library(tidyverse)
library(xlsx)
options(stringsAsFactors = F)

last_gw <- 20

for (n in 1:last_gw) {
  

  players_team <- team_round_17 %>% select(index,n+1)
  colnames(players_team)[2] <- "Team"
  
  players_club <- players_team %>% mutate(
    ARS = if_else(Team == "ARS",1,0),
    BHA = if_else(Team == "BHA",1,0),
    BOU = if_else(Team == "BOU",1,0),
    BUR = if_else(Team == "BUR",1,0),
    CHE = if_else(Team == "CHE",1,0),
    CRY = if_else(Team == "CRY",1,0),
    EVE = if_else(Team == "EVE",1,0),
    HUD = if_else(Team == "HUD",1,0),
    LEI = if_else(Team == "LEI",1,0),
    LIV = if_else(Team == "LIV",1,0),
    MCI = if_else(Team == "MCI",1,0),
    MUN = if_else(Team == "MUN",1,0),
    NEW = if_else(Team == "NEW",1,0),
    SOU = if_else(Team == "SOU",1,0),
    STK = if_else(Team == "STK",1,0),
    SWA = if_else(Team == "SWA",1,0),
    TOT = if_else(Team == "TOT",1,0),
    WAT = if_else(Team == "WAT",1,0),
    WBA = if_else(Team == "WBA",1,0),
    WHU = if_else(Team == "WHU",1,0)
  ) %>% select(ARS,BHA,BOU,BUR,CHE,CRY,EVE,HUD,LEI,LIV,MCI,MUN,NEW,SOU,STK,SWA,TOT,WAT,WBA,WHU)
  
  
  
  write.xlsx(players_club, "output/players_static_club.xlsx")
  
}
  
  