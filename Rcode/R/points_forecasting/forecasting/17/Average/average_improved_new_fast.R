#################
# Create Average Model w/ Elo new: allows a = 2
#################
library(tidyverse)


last_gw <- 35
a <- 4

#########
# Average w/ELO
#########

elo_team <- read.csv(file = "load/data_17/data_17_output/elo_team_17.csv")

# Fill Elo rating of opponents
elo_opponent <- read.csv(file = "load/data_17/data_17_output/elo_opponent_17.csv")

# Create Elo adjustment factor
elo_opponent <- data.matrix(elo_opponent)
elo_team <- data.matrix(elo_team)
elo_player_history <- elo_opponent/elo_team

# Adjust historic performance with Elo rating of opponent
points_round_17_elo <- points_round_17[,2:(last_gw+1)]*elo_player_history
index <- 1:625
points_round_17_elo <- cbind(index,points_round_17_elo)

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

# injuries
#############
injuries_17 <- read.csv(file = "load/data_17/data_17_output/injuries_17.csv")
injuries_17 <- data.matrix(injuries_17)
injuries_17 <- injuries_17[,2:(last_gw+1)]
score <- score*injuries_17
############

#For accuracy tests
forecasts_improved <- score

#################
# Write files
##################

write.csv(x = forecasts_improved,file = paste0("load/data_17/data_17_output/forecasts_improved_2017_hor_",a,".csv"),row.names = F)

library(xlsx) #does not work on mac per now
path <- paste0('../../../input/dynamic_data/season_17/forecasting_method/average/improved_hor_',a,'/')
#dir.create(path = path)

h<-11
elo_player_future <- elo_team/elo_opponent
# Add line adjusting for double gameweek elo (?)

for(week_for in 1:last_gw){
  
  predictions_table_average <- score %>% select(week_for)
  predictions_table_average[,2:(h)] <- predictions_table_average[,1]
  predictions_table_average <- predictions_table_average*elo_player_future[,(week_for):(h)]
  predictions_table_average <- cbind(points_average_a_17[,1],predictions_table_average)
  colnames(predictions_table_average)[1] <- "index"
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
