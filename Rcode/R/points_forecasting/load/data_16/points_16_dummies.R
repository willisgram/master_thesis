###############
# Points round dummies
###############

points_round_16 <- data.frame(index = 1:625)

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

#Round 1-4
#Get last round
data_5 <- read.csv("input/FPL16-GW5.csv")
data_5 <- data_5 %>% mutate(
  Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
  FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
)
data_5 <- full_join(data_5,players,by = c("FirstName_1" = "FirstName_1","Surname_1"="Surname_1"))
data_5 <- data_5[!is.na(data_5$index),]
data_5 <- data_5 %>% select(index,TotalPoints,PointsLastRound) %>% arrange(index)

points_round_16_1_4 <- points_round_16 %>% mutate(
  round_temp = data_5$TotalPoints - data_5$PointsLastRound,
  round_4 = round(round_temp/4),
  round_3 = round(round_temp/4),
  round_2 = round(round_temp/4),
  round_1 = round(round_temp/4)
) %>% select(round_1,round_2,round_3,round_4)

points_round_16 <- cbind(points_round_16_1_4,points_round_16)
points_round_16 <- points_round_16[,-5]
index = data.frame( index = 1:625)
points_round_16 <- cbind(index,points_round_16)

write.csv(x = points_round_16,file = "load/data_16/data_16_output/points_round_16.csv",row.names = F)
