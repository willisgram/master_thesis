#############################
# How to handle players not represented: Give them a 0 score AFTER regression is performed
#############################


###########################
#Check how well index matcing works
###########################


players_0 <- data_0 %>% mutate(
  Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
  FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
) %>% select(FirstName_1,Surname_1,Cost)

players_0 <- full_join(players_0,players,by = c("FirstName_1" = "FirstName_1","Surname_1"="Surname_1"))
players_0 <- players_0 %>% arrange(index)

players_1 <- data_1 %>% mutate(
  Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
  FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
) %>% select(FirstName_1,Surname_1,Cost)

players_1 <- full_join(players_1,players,by = c("FirstName_1" = "FirstName_1","Surname_1"="Surname_1"))
players_1 <- players_1 %>% arrange(index)

players_2 <- data_2 %>% mutate(
  Surname_1   = if_else(grepl(Surname,pattern = " "),sub('.* ', '', Surname),Surname),
  FirstName_1 = if_else(grepl(Surname,pattern = " "),sub(' .*', '',Surname ), FirstName)
) %>% select(FirstName_1,Surname_1,Cost)

players_2 <- full_join(players_2,players,by = c("FirstName_1" = "FirstName_1","Surname_1"="Surname_1"))
players_2 <- players_2 %>% arrange(index)









