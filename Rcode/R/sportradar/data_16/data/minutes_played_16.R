#1

###########
#Import training data
library(tidyverse)
options(stringsAsFactors = F)
folder <- "input/"
Minutes_played_round <- data.frame(index = 1:625)

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
  
  Minutes_played_round <- cbind(Minutes_played_round,data_temp$MinutesPlayed)
  colnames(Minutes_played_round)[i-3] <- paste0("round_",i)
  
}

library(xlsx)
write.xlsx(Minutes_played_round, "output/points_16.xlsx")



