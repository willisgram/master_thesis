################
#Complete player list
################
library(tidyverse)
options(stringsAsFactors = F)


#List after transfer window. Assume it to be enough
folder <- "input/"
year   <- "17"
week   <- "26"
sheet  <- paste0("FPL",year,"-GW",week,".csv")
path   <- paste0(folder,sheet)

data_26 <- read.csv(path)

players <- data_26 %>% mutate(
  Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
  FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
)


players <- players %>% arrange(match(PositionsList, c("GLK","DEF","MID","FWD")),Team) %>% mutate(
  index = 1:625
) %>% select(FirstName_1,Surname_1,PositionsList,Team,index) 

rm(list = c("data_26","folder","sheet","week","year","path"))

write.csv(x = players,file = "load/data_16/data_16_output/players.csv",row.names = F)


library(xlsx)
write.xlsx(players, "output/players_static.xlsx")


players_club <- players %>% mutate(
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






####################
#List of players after transfer:
#Possibly not needed
players_after <- read.csv("input/players.csv")
players_after <- players_after %>% 
  mutate(Surname = substr(players_after$name,start = 1,stop = (regexpr(pattern = ",",text = players_after$name))-1)
  )
players_after <- players_after %>% mutate(
  Surname = if_else(Surname == "",name,Surname)
)
####################















