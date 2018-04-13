#############
# Generate matrix of correlation
#############
library(stringr)
library(tidyverse)
options(stringsAsFactors = F)

# Same team
players_short <- players %>% group_by(PositionsList,Team) %>% summarise() %>% mutate(
  tag = paste(PositionsList,Team,sep = "_")
)

opponents_short <- opponents_17 %>% select(Team,GW1)
opponents_tagged <- inner_join(players,opponents_short,"Team")

players_short <- inner_join(players_short,opponents_short,"Team")
players_temp <- inner_join(players,players_short, by = c("PositionsList","Team"))

players_tagged <- inner_join(players_temp,opponents_tagged,by = c("index","Team","GW1")) %>% select(index,Team,tag,GW1)

cor_17 <- data.frame(matrix(nrow = 626,ncol = 626,0))
cor_17[1,2:626] <- players_tagged$tag
cor_17[2:626,1] <- players_tagged$tag

# Same poistion
for (i in 2:626) {
  for (j in 2:626) {
    cor_17[i,j] <- if_else(condition = cor_17[1,i] == cor_17[1,j],true = 1,false = 0)
  }
  
}

# GLK DEF same team
for (i in 2:626) {
  for (j in 2:626) {
    cor_17[i,j] <- if_else(
      condition = str_sub(cor_17[1,i],start = -3) == str_sub(cor_17[1,j],start = -3) & str_sub(cor_17[1,i],end = 3) == "GLK" & str_sub(cor_17[1,j],end = 3) == "DEF" ,true = 0.689,false = as.numeric(cor_17[i,j])
      )
  }
  
}
# DEF GLK same team
for (i in 2:626) {
  for (j in 2:626) {
    cor_17[i,j] <- if_else(
      condition = str_sub(cor_17[1,i],start = -3) == str_sub(cor_17[1,j],start = -3) & str_sub(cor_17[1,i],end = 3) == "DEF" & str_sub(cor_17[1,j],end = 3) == "GLK" ,true = 0.689,false = as.numeric(cor_17[i,j])
    )
  }
  
}


# GLK MID same team
for (i in 2:626) {
  for (j in 2:626) {
    cor_17[i,j] <- if_else(
      condition = str_sub(cor_17[1,i],start = -3) == str_sub(cor_17[1,j],start = -3) & str_sub(cor_17[1,i],end = 3) == "GLK" & str_sub(cor_17[1,j],end = 3) == "MID" ,true = 0.274,false = as.numeric(cor_17[i,j])
    )
  }
  
}
# MID GLK same team
for (i in 2:626) {
  for (j in 2:626) {
    cor_17[i,j] <- if_else(
      condition = str_sub(cor_17[1,i],start = -3) == str_sub(cor_17[1,j],start = -3) & str_sub(cor_17[1,i],end = 3) == "MID" & str_sub(cor_17[1,j],end = 3) == "GLK" ,true = 0.274,false = as.numeric(cor_17[i,j])
    )
  }
  
}


# DEF MID same team
for (i in 2:626) {
  for (j in 2:626) {
    cor_17[i,j] <- if_else(
      condition = str_sub(cor_17[1,i],start = -3) == str_sub(cor_17[1,j],start = -3) & str_sub(cor_17[1,i],end = 3) == "DEF" & str_sub(cor_17[1,j],end = 3) == "MID" ,true = 0.368,false = as.numeric(cor_17[i,j])
    )
  }
  
}
# MID DEF same team
for (i in 2:626) {
  for (j in 2:626) {
    cor_17[i,j] <- if_else(
      condition = str_sub(cor_17[1,i],start = -3) == str_sub(cor_17[1,j],start = -3) & str_sub(cor_17[1,i],end = 3) == "MID" & str_sub(cor_17[1,j],end = 3) == "DEF" ,true = 0.368,false = as.numeric(cor_17[i,j])
    )
  }
  
}


# MID FWD same team
for (i in 2:626) {
  for (j in 2:626) {
    cor_17[i,j] <- if_else(
      condition = str_sub(cor_17[1,i],start = -3) == str_sub(cor_17[1,j],start = -3) & str_sub(cor_17[1,i],end = 3) == "MID" & str_sub(cor_17[1,j],end = 3) == "FWD" ,true = 0.238,false = as.numeric(cor_17[i,j])
    )
  }
  
}
# FWD MID same team
for (i in 2:626) {
  for (j in 2:626) {
    cor_17[i,j] <- if_else(
      condition = str_sub(cor_17[1,i],start = -3) == str_sub(cor_17[1,j],start = -3) & str_sub(cor_17[1,i],end = 3) == "FWD" & str_sub(cor_17[1,j],end = 3) == "MID" ,true = 0.238,false = as.numeric(cor_17[i,j])
    )
  }
  
}


cor_team <- mutate_all(cor_17[2:626,2:626],as.numeric)
cor_test2 <- cor_17[,275:277]

write.csv(x = cor_team,file = "load/data_17/data_17_output/cor_team.csv",row.names = F)
write.csv(x = cor_17,file = "load/data_17/data_17_output/cor_17.csv",row.names = F)






