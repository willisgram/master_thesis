##################
# Home/away
##################
library(tidyverse)
library(xlsx)
options(stringsAsFactors = F)


#2016
path <- '../../../Data/BK/H.A.16.17.csv'

data_16 <- read.csv2(file = path,header = F)
colnames(data_16)[1] <- "Team"
colnames(data_16)[2:39] <- paste0("round_",1:38)

players_join <- players %>% select(index,Team)

h_b_16 <- full_join(players_join,data_16,"Team")
h_b_16 <- head(x = h_b_16,n=625)
h_b_16  <- h_b_16[,names(h_b_16) != "Team"]

write.csv(x = h_b_16,file = "load/data_16/data_16_output/h_b_16.csv",row.names = F)

#2017
path <- '../../../Data/BK/H.A.17.18.csv'

data_17 <- read.csv2(file = path,header = F)
colnames(data_17)[1] <- "Team"
colnames(data_17)[2:23] <- paste0("round_",1:22)
colnames(data_17)[24] <- paste0("d_round_",22)
colnames(data_17)[25:36] <- paste0("round_",23:34)
colnames(data_17)[37] <- paste0("d_round_",34)
colnames(data_17)[38:40] <- paste0("round_",35:37)
colnames(data_17)[41] <- paste0("d_round_",37)
colnames(data_17)[42] <- paste0("round_",38)

players_join <- players %>% select(index,Team)

h_b_17 <- inner_join(players_join,data_17,"Team")
h_b_17  <- h_b_17[,names(h_b_17) != "Team"]

# Home/away new (not w double gw explicit)
path <- '../../../Data/BK/H.A.17.18_short.csv'
h_a_17 <- read.csv2(file = path,header = T)
h_a_player <- data.frame(matrix(nrow = 625,ncol = 38,"W"))
team_round_17_short <- team_round_17[,names(team_round_17)!= "index"]

for (j in 1:38) {
  for (i in 1:625) {
    for (k in 1:20) {
      if(team_round_17_short[i,j] == h_a_17[k,1] & !is.na(team_round_17_short[i,j])){
        index <- k
        h_a_player[i,j] <- h_a_17[index,j+1]
        break
      }
    }
  }
}


write.csv(x = h_a_player,file = "load/data_17/data_17_output/h_a_17_player.csv",row.names = F)

# Home/away/double gw / no gw:

path <- '../../../Data/BK/H.A.D.17.18.csv'

gw_17 <- read.csv2(file = path,header = T)
gw_player <- data.frame(matrix(nrow = 625,ncol = 38,"W"))
team_round_17_short <- team_round_17[,names(team_round_17)!= "index"]

for (j in 1:38) {
  for (i in 1:625) {
    for (k in 1:20) {
      if(team_round_17_short[i,j] == gw_17[k,1] & !is.na(team_round_17_short[i,j])){
        index <- k
        gw_player[i,j] <- gw_17[index,j+1]
        break
      }
    }
  }
}

H <- 1
A <- 1
N <- 0
D <- 2

gw_player_num <- gw_player
gw_player_num[gw_player_num =="H"] <- as.numeric(H)
gw_player_num[gw_player_num =="A"] <- as.numeric(A)
gw_player_num[gw_player_num =="N"] <- as.numeric(N)
gw_player_num[gw_player_num =="D"] <- as.numeric(D)
gw_player_num[gw_player_num =="W"] <- NA

write.csv(x = gw_player_num,file = "load/data_17/data_17_output/gw_player_num.csv",row.names = F)









