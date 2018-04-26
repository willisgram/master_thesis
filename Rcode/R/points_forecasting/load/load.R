############
# Load verified static data
############
library(tidyverse)

#Players index
options(stringsAsFactors = F)
players <- read.csv(file = "load/data_all/players.csv")


###################
# Regressors 2016
###################

#Completed round data
#1-38
team_round_16 <- read.csv(file = "load/data_16/data_16_output/team_round_16.csv")
pos_round_16 <- read.csv(file = "load/data_16/data_16_output/pos_round_16.csv")
points_round_16 <- read.csv(file = "load/data_16/data_16_output/points_round_16.csv")
minutes_round_16 <- read.csv(file = "load/data_16/data_16_output/minutes_round_16.csv")
opponent_round_16 <- read.csv(file = "load/data_16/data_16_output/opponent_round_16.csv")
opponent_round_16_short <- read.csv(file = "load/data_16/data_16_output/opponent_round_16_short.csv")
h_a_16 <- read.csv(file = "load/data_16/data_16_output/h_a_16_player.csv")

#Completed w/ dummies
trans_in_round_16 <- read.csv(file = "load/data_16/data_16_output/trans_in_round_16.csv")
trans_out_round_16 <- read.csv(file = "load/data_16/data_16_output/trans_out_round_16.csv")
cost_round_16 <- read.csv(file = "load/data_16/data_16_output/cost_round_16.csv")

# Per team
path <- '../../../Data/BK/opponents.csv'
opponents_16 <- read.csv2(path)
opponents_16 <- opponents_16 %>% mutate_all(.funs = toupper)
######################

###################
# Regressors 2017
###################

#Completed overlord round data
#1-35
points_round_17 <- read.csv(file = "load/data_17/data_17_output/points_round_17.csv")
opponent_round_17 <- read.csv(file = "load/data_17/data_17_output/opponent_round_17.csv")
opponent_round_17_short <- read.csv(file = "load/data_17/data_17_output/opponent_round_17_short.csv")
cost_round_17 <- read.csv(file = "load/data_17/data_17_output/cost_round_17.csv")
team_round_17 <- read.csv(file = "load/data_17/data_17_output/team_round_17.csv")
pos_round_17 <- read.csv(file = "load/data_17/data_17_output/pos_round_17.csv")
trans_in_round_17 <- read.csv(file = "load/data_17/data_17_output/trans_in_round_17.csv")
trans_out_round_17 <- read.csv(file = "load/data_17/data_17_output/trans_out_round_17.csv")
minutes_round_17 <- read.csv(file = "load/data_17/data_17_output/minutes_round_17.csv")

#Other sources
h_a_17_double <- read.csv(file = "load/data_17/data_17_output/h_b_17.csv")
h_a_17 <- read.csv(file = "load/data_17/data_17_output/h_a_17_player.csv")
injuries_17 <- read.csv(file = "load/data_17/data_17_output/injuries_17.csv")
streak_matrix <- read.csv(file = "load/data_17/data_17_output/streak_matrix.csv")

#1-40
gw_player_num <- read.csv(file = "load/data_17/data_17_output/gw_player_num.csv")


# Per team
path <- '../../../Data/BK/opponents_17.csv'
opponents_17 <- read.csv2(path)
####################

#######################
#Write xlsx with Perfect information
######################
library(xlsx)
#2016
name_for_perf <- "forecast_point.xlsx"
path_for_perf <- "../../../input/dynamic_data/season_16/forecasting_method/perfect_information/"
file_for_perf <- paste0(path_for_perf, name_for_perf)

points_round_16[is.na(points_round_16)] <- -10000

write.xlsx(x = points_round_16,file = file_for_perf,row.names = F)

#2017
name_for_perf <- "forecast_point.xlsx"
path_for_perf <- "../../../input/dynamic_data/season_17/forecasting_method/perfect_information/"
file_for_perf <- paste0(path_for_perf, name_for_perf)

points_round_17[is.na(points_round_17)] <- -10000

write.xlsx(x = points_round_17,file = file_for_perf,row.names = F)
########################






