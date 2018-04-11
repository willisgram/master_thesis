#############
# Generate matrix of correlation
#############
library(stringr)
library(tidyverse)
options(stringsAsFactors = F)

#Round 1
cor_17 <- data.frame(matrix(nrow = 625,ncol = 625,0))
colnames(cor_17)[1:625] <- 1:625
diag(cor_17) <- 1

index <- data.frame(index = 1:625)
cor_17 <- cbind(index,cor_17)

cor_17 <- cbind(players,cor_17,"index")

# Same team
players_short <- players %>% group_by(PositionsList,Team) %>% summarise() %>% mutate(
  tag = paste(PositionsList,Team,sep = "_")
)

cor_temp <- as.data.frame(matrix(nrow = 80,ncol = 80,0))
colnames(cor_temp) <- players_short$key
cor_temp <- cbind(players_short,cor_temp)
cor_temp <- if_else(condition = cor_temp$ key)

opponents_short <- opponents_17 %>% select(Team,GW1)
opponents_tagged <- inner_join(players,opponents_short,"Team")

players_short <- inner_join(players_short,opponents_short,"Team")
players_temp <- inner_join(players,players_short, by = c("PositionsList","Team"))

players_tagged <- inner_join(players_temp,opponents_tagged,by = c("index","Team","GW1")) %>% select(index,Team,tag,GW1)

cor_17 <- data.frame(matrix(nrow = 626,ncol = 626,0))
cor_17[1,2:626] <- players_tagged$tag
cor_17[2:626,1] <- players_tagged$tag

cor_17[,] <- if_else(condition = cor_17[,1] == cor_17[1,],true = 1,false = 0)


# Same poistion
for (i in 2:626) {
  for (j in 2:626) {
    cor_17[i,j] <- if_else(condition = cor_17[1,i] == cor_17[1,j],true = 1,false = 0)
  }
  
}

# GK DEF same team
for (i in 2:626) {
  for (j in 2:626) {
    cor_17[i,j] <- if_else(
      condition = str_sub(cor_17[1,i],start = -3) == str_sub(cor_17[1,j],start = -3) & str_sub(cor_17[1,i],end = 3) == "GLK" & str_sub(cor_17[1,j],end = 3) == "DEF" ,true = 0.5,false = as.numeric(cor_17[i,j])
      )
  }
  
}














