#################
# Create Average Model w/ Elo
#################
library(tidyverse)


last_gw <- 38
a <- 4

#########
# Average w/ELO
#########

path <- '../../../Data/BK/ELO16-17_score.csv'
elo <- read.csv2(path)
elo_team <- data.frame(matrix(nrow = 625,ncol = last_gw,1))
team_round_16_short <- team_round_16[,names(team_round_16)!= "index"]
elo_opponent <- data.frame(matrix(nrow = 625,ncol = last_gw,1))
opponent_round_16_short <- opponent_round_16_short[,names(opponent_round_16_short) != "index"]

# Fill Elo rating of team
for (j in 1:last_gw) {
  for (i in 1:625) {
    for (k in 1:20) {
      if(team_round_16_short[i,j] == elo[k,1] & !is.na(team_round_16_short[i,j])){
        index <- k
        elo_team[i,j] <- elo[index,j+1]
        break
      }
    }
  }
}

# Fill Elo rating of opponents
for (j in 1:last_gw) {
  for (i in 1:625) {
    for (k in 1:20) {
      if(opponent_round_16_short[i,j] == elo[k,1] & !is.na(opponent_round_16_short[i,j])){
        index <- k
        elo_opponent[i,j] <- elo[index,j+1]
        break
      }
    }
  }
}

# Create Elo adjustment factor
elo_opponent <- data.matrix(elo_opponent)
elo_team <- data.matrix(elo_team)
elo_player_history <- elo_opponent/elo_team

# Adjust historic performance with Elo rating of opponent
points_round_16_elo <- points_round_16[,2:(last_gw+1)]*elo_player_history
index <- 1:625
points_round_16_elo <- cbind(index,points_round_16_elo)

# Calculate average Elo adjusted performance
points_average_a_16 <- data.frame(index = 1:625)
points_average_a_16[,2:(a+1)] <- rep(NA,625)
colnames(points_average_a_16)[2:(a+1)] <- paste0("round_",1:a)
#points_average_a_16 <- data.frame(index = 1:625,round_1 = rep(NA,625),round_2 = rep(NA,625),round_3 = rep(NA,625))

for(i in (a+1):last_gw){

    points_average_a_16[,i+1] <- rowMeans(points_round_16_elo[,((i+1)-a):(i)])
    
}
colnames(points_average_a_16)[(a+1):(last_gw+1)] <- paste0("round_",a:last_gw)
#########

########
# Home/Away
########

home <- 1.24
away <- 0.76

h_a_16_num <- h_a_16
h_a_16_num[h_a_16_num =="H"] <- as.numeric(home)
h_a_16_num[h_a_16_num =="A"] <- as.numeric(away)
h_a_16_num[h_a_16_num =="W"] <- NA
h_a_16_num <- data.matrix(h_a_16_num)

score <- points_average_a_16[,2:(last_gw+1)]*h_a_16_num[,1:(last_gw)]
##########

########
# Player Point Streak
########
X <- 4 #stricly higher
Y <- 2 #strictly less

streak_matrix <- data.frame(matrix(nrow = 625,ncol = 40,1))
points_16 <- points_round_16[,names(points_round_16) != "index"]

## Positive streak
for (i in 1:625) {
  for (j in 2:last_gw) {
    if(j>5){
      if(all(points_16[i,(j-5):(j-1)]>X) & streak_matrix[i,j] == 1 & all(!is.na(points_16[i,(j-5):(j-1)]))){
        streak_matrix[i,j] <- 1.05
      }
    }
    if(j>4){
      if(all(points_16[i,(j-4):(j-1)]>X) & streak_matrix[i,j] == 1 & all(!is.na(points_16[i,(j-4):(j-1)]))){
        streak_matrix[i,j] <- 1.04
      }
    }
    if(j>3){
      if(all(points_16[i,(j-3):(j-1)]>X) & streak_matrix[i,j] == 1 & all(!is.na(points_16[i,(j-3):(j-1)]))){
        streak_matrix[i,j] <- 1.03
      }
    }
    if(j>2){
      if(all(points_16[i,(j-2):(j-1)]>X) & streak_matrix[i,j] == 1 & all(!is.na(points_16[i,(j-2):(j-1)]))){
        streak_matrix[i,j] <- 1.02
      }
    }
    if(j>1){
      if(all(points_16[i,(j-1):(j-1)]>X) & streak_matrix[i,j] == 1 & all(!is.na(points_16[i,(j-1):(j-1)]))){
        streak_matrix[i,j] <- 1.01
      }
    }
  }
}

## Negative streak
for (i in 1:625) {
  for (j in 2:last_gw) {
    if(j>5){
      if(all(points_16[i,(j-5):(j-1)]<Y) & streak_matrix[i,j] == 1 & all(!is.na(points_16[i,(j-5):(j-1)]))){
        streak_matrix[i,j] <- 0.95
      }
    }
    if(j>4){
      if(all(points_16[i,(j-4):(j-1)]<Y) & streak_matrix[i,j] == 1 & all(!is.na(points_16[i,(j-4):(j-1)]))){
        streak_matrix[i,j] <- 0.96
      }
    }
    if(j>3){
      if(all(points_16[i,(j-3):(j-1)]<Y) & streak_matrix[i,j] == 1 & all(!is.na(points_16[i,(j-3):(j-1)]))){
        streak_matrix[i,j] <- 0.97
      }
    }
    if(j>2){
      if(all(points_16[i,(j-2):(j-1)]<Y) & streak_matrix[i,j] == 1 & all(!is.na(points_16[i,(j-2):(j-1)]))){
        streak_matrix[i,j] <- 0.98
      }
    }
    if(j>1){
      if(all(points_16[i,(j-1):(j-1)]<Y) & streak_matrix[i,j] == 1 & all(!is.na(points_16[i,(j-1):(j-1)]))){
        streak_matrix[i,j] <- 0.99
      }
    }
  }
}

score <- score*streak_matrix[,1:last_gw]

#########

# Dobule/no gameweek
#############
#gw_player_num <- data.matrix(gw_player_num)
#score <- score*gw_player_num[,1:last_gw]
############

# injuries
#############
#injuries_17 <- data.matrix(injuries_17)
#injuries_17 <- injuries_17[,2:(last_gw+1)]
#score <- score*injuries_17
############

#For accuracy tests
forecasts_improved <- score

#################
# Write files
##################

write.csv(x = forecasts_improved,file = paste0("load/data_16/data_16_output/forecasts_improved_2016_hor_",a,".csv"),row.names = F)

library(xlsx) #does not work on mac per now

h<-11
elo_player_future <- elo_team/elo_opponent
# Add line adjusting for double gameweek elo (?)

for(week_for in (a+1):last_gw){
  
  predictions_table_average <- score %>% select(week_for)
  predictions_table_average[,2:(h)] <- predictions_table_average[,1]
  predictions_table_average <- predictions_table_average*elo_player_future[,(week_for):(h)]
  predictions_table_average <- cbind(points_average_a_16[,1],predictions_table_average)
  colnames(predictions_table_average)[1] <- "index"
  colnames(predictions_table_average)[2:(h+1)] <- paste0("round_",(week_for):(week_for+h-1))
  predictions_table_average[is.na(predictions_table_average)] <- -10000
  
  # Assign name
  #Forecasts
  name_for_avg <- paste0("forecast_point_GW", as.character(week_for-(a)),".xlsx") #adjust for missing 3
  path_for_avg <- paste0('../../../input/dynamic_data/season_16/forecasting_method/average/improved_hor_',a,'/')
  file_for_avg <- paste0(path_for_avg, name_for_avg)
  
  #assign(x = name_for,value = predictions_table_average)
  
  # Write xlsx file
  rownames(predictions_table_average) <- NULL
  write.xlsx(predictions_table_average, file_for_avg,row.names = F)
  
  
}
