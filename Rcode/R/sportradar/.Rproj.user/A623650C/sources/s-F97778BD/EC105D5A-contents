#1

###########
#Import training data
library(tidyverse)
options(stringsAsFactors = F)
folder <- "input/"


for( i in 0:12){
  year     <- "17"
  week_0   <- as.character(i)
  week_1   <- as.character(i+1)
  sheet_0  <- paste0("FPL",year,"-GW",week_0,".csv")
  sheet_1  <- paste0("FPL",year,"-GW",week_1,".csv")
  path_0   <- paste0(folder,sheet_0)
  path_1   <- paste0(folder,sheet_1)
  
  data_temp_0   <- read.csv(path_0)
  data_temp_1   <- read.csv(path_1)
  
  data_temp_0 <- data_temp_0 %>% mutate(
    Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
    FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
  )
  
  data_temp_1 <- data_temp_1 %>% mutate(
    Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
    FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
  )
  
  
  players_temp_0 <- data_temp_0 %>% mutate(
    Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
    FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
  ) %>% select(FirstName_1,Surname_1,NextFixture1)
  
  players_temp_0 <- full_join(players_temp_0,players,by = c("FirstName_1" = "FirstName_1","Surname_1"="Surname_1"))
  players_temp_0 <- players_temp_0 %>% select(index,NextFixture1)
  
  players_temp_1 <- data_temp_1 %>% mutate(
    Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
    FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
  ) %>% select(FirstName_1,Surname_1,Cost,PointsLastRound,TransfersOutRound,TransfersInRound)
  
  players_temp_1 <- full_join(players_temp_1,players,by = c("FirstName_1" = "FirstName_1","Surname_1"="Surname_1"))
  players_temp_1 <- full_join(players_temp_1,players_temp_0,by = "index")
  players_temp_1 <- players_temp_1 %>% mutate(
    opponent = NextFixture1,
    points   = PointsLastRound
  ) %>% select(index,Team,PositionsList,Cost,points,TransfersInRound,TransfersOutRound,opponent
               ) %>% arrange(index) %>% na.omit()
 
  name <- paste0("regressors_",i+1)
  
  assign(name,players_temp_1)
   
}
