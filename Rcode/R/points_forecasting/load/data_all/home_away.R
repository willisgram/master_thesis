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

write.csv(x = h_b_17,file = "load/data_17/data_17_output/h_b_17.csv",row.names = F)

