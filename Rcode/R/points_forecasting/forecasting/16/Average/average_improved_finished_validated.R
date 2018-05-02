#################
# Create Average Model w/ Elo
#################
library(tidyverse)


last_gw <- 38
a <- 8

#########
# Average w/ELO
#########
elo_opponent <- data.matrix(elo_opponent_16)
elo_team <- data.matrix(elo_team_16)
elo_player_history <- elo_opponent/elo_team


# Adjust historic performance with Elo rating of opponent
points_round_16_elo <- points_round_16*elo_player_history

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
streak_matrix_16 <- read.csv(file = "load/data_16/data_16_output/streak_matrix_16.csv")
streak_matrix_16 <- data.matrix(streak_matrix_16)
score <- score*streak_matrix_16

#########

#For accuracy tests
forecasts_improved <- score

#################
# Write files
##################

write.csv(x = forecasts_improved,file = paste0("load/data_16/data_16_output/forecasts_improved_2016_hor_",a,".csv"),row.names = F)

library(xlsx) #does not work on mac per now

h<-11
#elo_player_future <- elo_team/elo_opponent
# Add line adjusting for double gameweek elo (?)


team_round_16_short <- team_round_16[,names(team_round_16)!= "index"]

for(week_for in (a+1):last_gw){
  
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
        if(team_round_16_short[i,j] == elo_16[k,1] & !is.na(team_round_16_short[i,j])){
          index <- k
          elo_team_temp[i,(j-week_for+1)] <- elo_16[index,week_for+1]
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
        if(opponent_round_16_short[i,j] == elo_16[k,1] & !is.na(opponent_round_16_short[i,j])){
          index <- k
          elo_opponent_temp[i,(j-week_for+1)] <- elo_16[index,week_for+1]
          break
        }
      }
    }
  }
  
  elo_team_temp <- data.matrix(elo_team_temp)
  elo_opponent_temp <- data.matrix(elo_opponent_temp)
  elo_player_future_temp <- elo_team_temp/elo_opponent_temp
  
  
  predictions_table_average <- score %>% select(week_for)
  predictions_table_average[,2:(h)] <- predictions_table_average[,1]
  predictions_table_average <- predictions_table_average*elo_player_future_temp
  index <- 1:625
  predictions_table_average <- cbind(index,predictions_table_average)
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
