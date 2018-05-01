#############
# Fill matrix of correlation with opponontes each round
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

cor_opp <- data.frame(matrix(nrow = 627,ncol = 627,0))
cor_opp[1,3:627] <- players_tagged$tag
cor_opp[3:627,1] <- players_tagged$tag
cor_opp[2,3:627] <- players_tagged$GW1
cor_opp[3:627,2] <- players_tagged$GW1

cor_opp[3:627,3:627] <- cor_team

##FILL
# GLK DEF opponents round 1
for (i in 3:627) {
  for (j in 3:627) {
    cor_opp[i,j] <- if_else(
      condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "GLK" & str_sub(cor_opp[1,j],end = 3) == "DEF" ,true = -0.106,false = as.numeric(cor_opp[i,j])
    )
  }
  
}
# DEF GLK opponents round 1
for (i in 3:627) {
  for (j in 3:627) {
    cor_opp[i,j] <- if_else(
      condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "DEF" & str_sub(cor_opp[1,j],end = 3) == "GLK" ,true = -0.106,false = as.numeric(cor_opp[i,j])
    )
  }
  
}

# GLK MID opponents round 1
for (i in 3:627) {
  for (j in 3:627) {
    cor_opp[i,j] <- if_else(
      condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "GLK" & str_sub(cor_opp[1,j],end = 3) == "MID" ,true = -0.312,false = as.numeric(cor_opp[i,j])
    )
  }
  
}
# MID GLK opponents round 1
for (i in 3:627) {
  for (j in 3:627) {
    cor_opp[i,j] <- if_else(
      condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "MID" & str_sub(cor_opp[1,j],end = 3) == "GLK" ,true = -0.312,false = as.numeric(cor_opp[i,j])
    )
  }
  
}

# GLK FWD opponents round 1
for (i in 3:627) {
  for (j in 3:627) {
    cor_opp[i,j] <- if_else(
      condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "GLK" & str_sub(cor_opp[1,j],end = 3) == "FWD" ,true = -0.336,false = as.numeric(cor_opp[i,j])
    )
  }
  
}
# FWD GLK opponents round 1
for (i in 3:627) {
  for (j in 3:627) {
    cor_opp[i,j] <- if_else(
      condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "MID" & str_sub(cor_opp[1,j],end = 3) == "FWD" ,true = -0.336,false = as.numeric(cor_opp[i,j])
    )
  }
  
}

# DEF DEF opponents round 1
for (i in 3:627) {
  for (j in 3:627) {
    cor_opp[i,j] <- if_else(
      condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "DEF" & str_sub(cor_opp[1,j],end = 3) == "DEF" ,true = -0.319,false = as.numeric(cor_opp[i,j])
    )
  }
  
}

# DEF MID opponents round 1
for (i in 3:627) {
  for (j in 3:627) {
    cor_opp[i,j] <- if_else(
      condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "DEF" & str_sub(cor_opp[1,j],end = 3) == "MID" ,true = -0.447,false = as.numeric(cor_opp[i,j])
    )
  }
  
}
# MID DEF opponents round 1
for (i in 3:627) {
  for (j in 3:627) {
    cor_opp[i,j] <- if_else(
      condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "MID" & str_sub(cor_opp[1,j],end = 3) == "DEF" ,true = -0.447,false = as.numeric(cor_opp[i,j])
    )
  }
  
}

# DEF FWD opponents round 1
for (i in 3:627) {
  for (j in 3:627) {
    cor_opp[i,j] <- if_else(
      condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "DEF" & str_sub(cor_opp[1,j],end = 3) == "FWD" ,true = -0.292,false = as.numeric(cor_opp[i,j])
    )
  }
  
}
# FWD DEF opponents round 1
for (i in 3:627) {
  for (j in 3:627) {
    cor_opp[i,j] <- if_else(
      condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "FWD" & str_sub(cor_opp[1,j],end = 3) == "DEF" ,true = -0.292,false = as.numeric(cor_opp[i,j])
    )
  }
  
}

# MID MID opponents round 1
for (i in 3:627) {
  for (j in 3:627) {
    cor_opp[i,j] <- if_else(
      condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "MID" & str_sub(cor_opp[1,j],end = 3) == "MID" ,true = -0.254,false = as.numeric(cor_opp[i,j])
    )
  }
  
}

# MID FWD opponents round 1
for (i in 3:627) {
  for (j in 3:627) {
    cor_opp[i,j] <- if_else(
      condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "MID" & str_sub(cor_opp[1,j],end = 3) == "FWD" ,true = -0.136,false = as.numeric(cor_opp[i,j])
    )
  }
  
}
# FWD MID opponents round 1
for (i in 3:627) {
  for (j in 3:627) {
    cor_opp[i,j] <- if_else(
      condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "FWD" & str_sub(cor_opp[1,j],end = 3) == "MID" ,true = -0.136,false = as.numeric(cor_opp[i,j])
    )
  }
  
}

# FWD FWD opponents round 1
for (i in 3:627) {
  for (j in 3:627) {
    cor_opp[i,j] <- if_else(
      condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "FWD" & str_sub(cor_opp[1,j],end = 3) == "FWD" ,true = -0.119,false = as.numeric(cor_opp[i,j])
    )
  }
  
}



cor_opp_test <- cor_opp[1:5,]








