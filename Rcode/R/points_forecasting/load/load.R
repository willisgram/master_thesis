############
# Load verified static data
############


#Players index
players <- read.csv(file = "load/data_all/players.csv")


###################
# Regressors
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


