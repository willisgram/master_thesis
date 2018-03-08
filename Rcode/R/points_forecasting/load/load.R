############
# Load verified static data
############

#Players index
players <- read.csv(file = "data_16/data_16_output/players.csv")


#Completed round data
#1-38
team_round_16 <- read.csv(file = "data_16/data_16_output/team_round_16.csv")
pos_round_16 <- read.csv(file = "data_16/data_16_output/pos_round_16.csv")

# Missing rounds data
#5-38
points_round_16 <- read.csv(file = "data_16/data_16_output/points_round_16.csv")
cost_round_16 <- read.csv(file = "data_16/data_16_output/cost_round_16.csv") #spanish, 37 = 38

#6-38
opponent_round_16 <- read.csv(file = "data_16/data_16_output/opponent_round_16.csv")


#Completed w/ dummies
trans_in_round_16 <- read.csv(file = "data_16/data_16_output/trans_in_round_16.csv")
trans_out_round_16 <- read.csv(file = "data_16/data_16_output/trans_out_round_16.csv")