############
# Load verified static data
############


#Players index
players <- read.csv(file = "load/data_all/players.csv")


###################
# Regressors 2016
###################

#Completed round data
#1-38
team_round_16 <- read.csv(file = "load/data_16/data_16_output/team_round_16.csv")
pos_round_16 <- read.csv(file = "load/data_16/data_16_output/pos_round_16.csv")

#Completed w/ dummies
trans_in_round_16 <- read.csv(file = "load/data_16/data_16_output/trans_in_round_16.csv")
trans_out_round_16 <- read.csv(file = "load/data_16/data_16_output/trans_out_round_16.csv")
minutes_round_16 <- read.csv(file = "load/data_16/data_16_output/minutes_round_16.csv")
cost_round_16 <- read.csv(file = "load/data_16/data_16_output/cost_round_16.csv")
points_round_16 <- read.csv(file = "load/data_16/data_16_output/points_round_16.csv")
opponent_round_16 <- read.csv(file = "load/data_16/data_16_output/opponent_round_16.csv")

# Missing rounds data:
#Points: 1-4 #Possible to fix
#Trans: 1-4 and 38 #Not possible to fix
#Cost: 1-4 and 38 #Almost possible to fix
#Minutes 1-4 and 38 (weird at 37) #Possible to fix
#Opponent 1-5 #Possible to fix

###################
# Regressors 2017
###################

#Completed round data
#1-27
team_round_17 <- read.csv(file = "load/data_17/data_17_output/team_round_17.csv")
pos_round_17 <- read.csv(file = "load/data_17/data_17_output/pos_round_17.csv")
trans_in_round_17 <- read.csv(file = "load/data_17/data_17_output/trans_in_round_17.csv")
trans_out_round_17 <- read.csv(file = "load/data_17/data_17_output/trans_out_round_17.csv")
minutes_round_17 <- read.csv(file = "load/data_17/data_17_output/minutes_round_17.csv")
cost_round_17 <- read.csv(file = "load/data_17/data_17_output/cost_round_17.csv")
points_round_17 <- read.csv(file = "load/data_17/data_17_output/points_round_17.csv")
opponent_round_17 <- read.csv(file = "load/data_17/data_17_output/opponent_round_17.csv")




#Write xlsx with Perfect information
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









