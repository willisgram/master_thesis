#############################
# How to handle players not represented: Give them a 0 score AFTER regression is performed
#############################


#1
players_0 <- data_0 %>% mutate(
  Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
  FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
) %>% select(FirstName_1,Surname_1,NextFixture1)

players_0 <- full_join(players_0,players,by = c("FirstName_1" = "FirstName_1","Surname_1"="Surname_1"))
players_0 <- players_0 %>% select(index,NextFixture1)

players_1 <- data_1 %>% mutate(
  Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
  FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
) %>% select(FirstName_1,Surname_1,Cost,PointsLastRound,TransfersOutRound,TransfersInRound)

players_1 <- full_join(players_1,players,by = c("FirstName_1" = "FirstName_1","Surname_1"="Surname_1"))
players_1 <- full_join(players_1,players_0,by = "index")
players_1 <- players_1 %>% mutate(
  opponent = NextFixture1,
  points   = PointsLastRound
  ) %>% select(Team,PositionsList,Cost,points,TransfersInRound,TransfersOutRound,opponent) %>% na.omit()

#2
players_1_2 <- data_1 %>% mutate(
  Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
  FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
) %>% select(FirstName_1,Surname_1,NextFixture1)

players_1_2 <- full_join(players_1_2,players,by = c("FirstName_1" = "FirstName_1","Surname_1"="Surname_1"))
players_1_2 <- players_1_2 %>% select(index,NextFixture1)

players_2 <- data_2 %>% mutate(
  Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
  FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
) %>% select(FirstName_1,Surname_1,Cost,PointsLastRound,TransfersOutRound,TransfersInRound)

players_2 <- full_join(players_2,players,by = c("FirstName_1" = "FirstName_1","Surname_1"="Surname_1"))
players_2 <- full_join(players_2,players_1_2,by = "index")
players_2 <- players_2 %>% mutate(
  opponent = NextFixture1,
  points   = PointsLastRound
) %>% select(Team,PositionsList,Cost,points,TransfersInRound,TransfersOutRound,opponent) %>% na.omit()
