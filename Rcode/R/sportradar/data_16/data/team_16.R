###########
#Import training data
library(tidyverse)
options(stringsAsFactors = F)
folder <- "input/"
team_round <- data.frame(index = 1:625)

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
  
  team_round <- cbind(team_round,data_temp$Team.x)
  colnames(team_round)[i-3] <- paste0("round_",i)
  
}
