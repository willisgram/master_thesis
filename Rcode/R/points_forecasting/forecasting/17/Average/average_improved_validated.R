#################
# Create Average Model w/ Elo new: allows a = 2
#################
library(tidyverse)


last_gw <- 35
a <- 3


#########
# Average w/ELO
#########
elo_opponent <- data.matrix(elo_opponent_17)
elo_team <- data.matrix(elo_team_17)
elo_player_history <- elo_opponent/elo_team
ones <- rep(1,625)
elo_player_history <- cbind(ones,elo_player_history)

# Adjust historic performance with Elo rating of opponent
points_round_17_elo <- points_round_17*elo_player_history


# Calculate average Elo adjusted performance
points_average_a_17 <- data.frame(index = 1:625)

for(i in 1:last_gw){
  
  if(i <= a){
    if(i == 1){
      points_average_a_17[,i+1] <- rowMeans(points_round_16[,(39+(i)-a):(39)])
    } else if(i == 2 & a != 2){
      points_average_a_17[,i+1] <- (rowSums(points_round_16[,(39+(i)-a):(39)]) + points_round_17_elo[,2])/a
    } else if(i == a & a != 2){
      points_average_a_17[,i+1] <- (points_round_16[,39] + rowSums(points_round_17_elo[,2:(i)]))/a
    } else if(i == 2 & a == 2){
      points_average_a_17[,i+1] <- (points_round_16[,39] + points_round_17_elo[,2])/a
    } else{
      points_average_a_17[,i+1] <- (rowSums(points_round_16[,(39+(i)-a):(39)]) + rowSums(points_round_17_elo[,2:(i)]))/a
    }
    
    
  } else{
    
    points_average_a_17[,i+1] <- rowMeans(points_round_17_elo[,((i+1)-a):(i)])
    
  }    
  
}
colnames(points_average_a_17)[2:(last_gw+1)] <- paste0("round_",1:last_gw)
#########

########
# Home/Away
########

home <- 1.24
away <- 0.76

h_a_17_num <- h_a_17
h_a_17_num[h_a_17_num =="H"] <- as.numeric(home)
h_a_17_num[h_a_17_num =="A"] <- as.numeric(away)
h_a_17_num[h_a_17_num =="W"] <- NA
h_a_17_num <- data.matrix(h_a_17_num)

score <- points_average_a_17[,2:(last_gw+1)]*h_a_17_num[,1:(last_gw)]
##########

########
# Player Point Streak
########
streak_matrix <- read.csv(file = "load/data_17/data_17_output/streak_matrix.csv")
score <- score*streak_matrix[,1:last_gw]

#########

# Dobule/no gameweek
#############
gw_player_num <- data.matrix(gw_player_num)
score <- score*gw_player_num[,1:last_gw]
############

library(xlsx) #does not work on mac per now
path <- paste0('../../../input/dynamic_data/season_17/forecasting_method/average/improved_hor_',a,'/')
#dir.create(path = path )

h<-11
team_round_17_short <- team_round_17[,names(team_round_17)!= "index"]
opponent_round_17_short <- opponent_round_17_short[,names(opponent_round_17_short) != "index"]

for(week_for in 1:last_gw){
  
  if(last_gw-week_for < h){
    n <- last_gw-week_for+1
  } else{
    n <- h
  }
  
  elo_team_temp <- data.frame(matrix(nrow = 625,ncol = h,1))
  
  # Fill Elo rating of team
  for (j in week_for:(week_for+n-1)) {
    for (i in 1:625) {
      for (k in 1:20) {
        if(team_round_17_short[i,j] == elo_17[k,1] & !is.na(team_round_17_short[i,j])){
          index <- k
          elo_team_temp[i,(j-week_for+1)] <- elo_17[index,week_for+1]
          break
        }
      }
    }
  }
  
  elo_opponent_temp <- data.frame(matrix(nrow = 625,ncol = h,1))
  
  # Fill Elo rating of opponent
  for (j in week_for:(week_for+n-1)) {
    for (i in 1:625) {
      for (k in 1:20) {
        if(opponent_round_17_short[i,j] == elo_17[k,1] & !is.na(opponent_round_17_short[i,j])){
          index <- k
          elo_opponent_temp[i,(j-week_for+1)] <- elo_17[index,week_for+1]
          break
        }
      }
    }
  }
  
  elo_team_temp <- data.matrix(elo_team_temp)
  elo_opponent_temp <- data.matrix(elo_opponent_temp)
  elo_player_future_temp <- elo_team_temp/elo_opponent_temp
  
  injuries_temp <- injuries_17 %>% select(week_for+1)
  injuries_temp[,2:(h)] <- injuries_temp[,1]
  
  predictions_table_average <- score %>% select(week_for)
  predictions_table_average[,2:(h)] <- predictions_table_average[,1]
  predictions_table_average <- predictions_table_average*elo_player_future_temp
  predictions_table_average <- predictions_table_average*injuries_temp
  
  index <- 1:625
  predictions_table_average <- cbind(index,predictions_table_average)
  colnames(predictions_table_average)[2:(h+1)] <- paste0("round_",(week_for):(week_for+h-1))
  predictions_table_average[is.na(predictions_table_average)] <- -10000
  
  # Assign name
  #Forecasts
  name_for_avg <- paste0("forecast_point_GW", as.character(week_for),".xlsx")
  path_for_avg <- path
  file_for_avg <- paste0(path_for_avg, name_for_avg)
  
  #assign(x = name_for,value = predictions_table_average)
  
  # Write xlsx file
  rownames(predictions_table_average) <- NULL
  write.xlsx(predictions_table_average, file_for_avg,row.names = F)
  
  
}
