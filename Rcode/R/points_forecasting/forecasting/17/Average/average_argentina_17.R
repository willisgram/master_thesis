#################
# Recreate Argentinian Method
#################
library(tidyverse)

a <- 3
last_gw <- 29
points_average_a_17 <- data.frame(index = 1:625)

#########
# Average
#########

for(i in 1:last_gw){
  
  if(i <= a){
    if(i == 1){
      points_average_a_17[,i+1] <- rowMeans(points_round_16[,(39+(i)-a):(39)])
    } else if(i == 2){
      points_average_a_17[,i+1] <- (rowSums(points_round_16[,(39+(i)-a):(39)]) + points_round_17[,2])/a
    } else if(i == a){
      points_average_a_17[,i+1] <- (points_round_16[,39] + rowSums(points_round_17[,2:(i)]))/a
    } else{
      points_average_a_17[,i+1] <- (rowSums(points_round_16[,(39+(i)-a):(39)]) + rowSums(points_round_17[,2:(i)]))/a
    }
    
    
  } else{
    
    points_average_a_17[,i+1] <- rowMeans(points_round_17[,((i+1)-a):(i)])
    
  }    
  
}
colnames(points_average_a_17)[2:(last_gw+1)] <- paste0("round_",1:last_gw)
#########

########
# Home/Away
########

home <- 1.05
away <- 0.95

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
X <- 4 #stricly higher
Y <- 2 #strictly less

streak_matrix <- data.frame(matrix(nrow = 625,ncol = 40,1))
points_17 <- points_round_17[,names(points_round_17) != "index"]

## Positive streak
for (i in 1:625) {
  for (j in 2:last_gw) {
    if(j>5){
      if(all(points_17[i,(j-5):(j-1)]>X) & streak_matrix[i,j] == 1 & all(!is.na(points_17[i,(j-5):(j-1)]))){
        streak_matrix[i,j] <- 1.05
      }
    }
    if(j>4){
      if(all(points_17[i,(j-4):(j-1)]>X) & streak_matrix[i,j] == 1 & all(!is.na(points_17[i,(j-4):(j-1)]))){
        streak_matrix[i,j] <- 1.04
      }
    }
    if(j>3){
      if(all(points_17[i,(j-3):(j-1)]>X) & streak_matrix[i,j] == 1 & all(!is.na(points_17[i,(j-3):(j-1)]))){
        streak_matrix[i,j] <- 1.03
      }
    }
    if(j>2){
      if(all(points_17[i,(j-2):(j-1)]>X) & streak_matrix[i,j] == 1 & all(!is.na(points_17[i,(j-2):(j-1)]))){
        streak_matrix[i,j] <- 1.02
      }
    }
    if(j>1){
      if(all(points_17[i,(j-1):(j-1)]>X) & streak_matrix[i,j] == 1 & all(!is.na(points_17[i,(j-1):(j-1)]))){
        streak_matrix[i,j] <- 1.01
      }
    }
  }
}

## Negative streak
for (i in 1:625) {
  for (j in 2:last_gw) {
    if(j>5){
      if(all(points_17[i,(j-5):(j-1)]<Y) & streak_matrix[i,j] == 1 & all(!is.na(points_17[i,(j-5):(j-1)]))){
        streak_matrix[i,j] <- 0.95
      }
    }
    if(j>4){
      if(all(points_17[i,(j-4):(j-1)]<Y) & streak_matrix[i,j] == 1 & all(!is.na(points_17[i,(j-4):(j-1)]))){
        streak_matrix[i,j] <- 0.96
      }
    }
    if(j>3){
      if(all(points_17[i,(j-3):(j-1)]<Y) & streak_matrix[i,j] == 1 & all(!is.na(points_17[i,(j-3):(j-1)]))){
        streak_matrix[i,j] <- 0.97
      }
    }
    if(j>2){
      if(all(points_17[i,(j-2):(j-1)]<Y) & streak_matrix[i,j] == 1 & all(!is.na(points_17[i,(j-2):(j-1)]))){
        streak_matrix[i,j] <- 0.98
      }
    }
    if(j>1){
      if(all(points_17[i,(j-1):(j-1)]<Y) & streak_matrix[i,j] == 1 & all(!is.na(points_17[i,(j-1):(j-1)]))){
        streak_matrix[i,j] <- 0.99
      }
    }
  }
}

score <- score*streak_matrix[,1:last_gw]




########

# Team Ranking
########
path <- '../../../Data/BK/League-table.csv'

ranking_17 <- read.csv2(file = path,header = T)
ranking_player <- data.frame(matrix(nrow = 625,ncol = 38,10))
team_round_17_short <- team_round_17[,names(team_round_17)!= "index"]

for (j in 1:last_gw) {
  for (i in 1:625) {
    for (k in 1:20) {
      if(team_round_17_short[i,j] == ranking_17[k,1] & !is.na(team_round_17_short[i,j])){
        index <- k
        ranking_player[i,j] <- ranking_17[index,j+1]
        break
      }
    }
  }
}
ranking_player <- data.matrix(ranking_player)
score <- score*ranking_player[,1:last_gw]

#########

# Dobule/no gameweek
#############
gw_player_num <- data.matrix(gw_player_num)
score <- score*gw_player_num[,1:last_gw]
############

# Injuries
#############
injuries_17 <- data.matrix(injuries_17)
injuries_17 <- injuries_17[,2:(last_gw+1)]
score <- score*injuries_17

############
#For accuracy tests
forecasts_argentina <- score

#################
# Write files
##################
library(xlsx) #does not work on mac per now

#s <- 1+a #first prediction with data
h<-11
score <- cbind(points_average_a_17[,1],score)
colnames(score)[1] <- "index"

for(week_for in 1:last_gw){
  
  predictions_table_average <- score %>% select(index,week_for+1)
  predictions_table_average[,3:(h+1)] <- predictions_table_average[,2]
  colnames(predictions_table_average)[2:(h+1)] <- paste0("round_",(week_for):(week_for+h-1))
  predictions_table_average[is.na(predictions_table_average)] <- -10000
  
  # Assign name
  #Forecasts
  name_for_avg <- paste0("forecast_point_GW", as.character(week_for),".xlsx")
  path_for_avg <- '../../../input/dynamic_data/season_17/forecasting_method/average/argentina/'
  file_for_avg <- paste0(path_for_avg, name_for_avg)
  
  #assign(x = name_for,value = predictions_table_average)
  
  # Write xlsx file
  rownames(predictions_table_average) <- NULL
  write.xlsx(predictions_table_average, file_for_avg,row.names = F)
  
  
}
