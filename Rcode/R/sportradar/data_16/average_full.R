#################
# Pred = average from a preveious games
#################


################
# Create dataframe
################
a <- 3
points_average_a_16 <- data.frame(index = 1:625)

points_average_a_16[,2:5] <- NA

for(i in 5:37){
  
  if(i <= a+4){
  
  points_average_a_16[,i+1] <- NA
      
  } else{
    
    points_average_a_16[,i+1] <- rowMeans(points_round_16[,((i-3)-a):(i-4)])
    
  }    
  
}
colnames(points_average_a_16)[2:38] <- paste0("round_",1:37)

############


#################
# Write files
##################
library(xlsx) #does not work on mac per now

h<-10

for(week_for in 5:37){
  
  predictions_table_average <- points_average_a_16 %>% select(index,week_for+1)
  predictions_table_average[,3:(h+1)] <- predictions_table_average[,2]
  colnames(predictions_table_average)[2:(h+1)] <- paste0("round_",(week_for):(week_for+h-1))
  
  # Assign name
  #Forecasts
  name_for_avg <- paste0("forecast_point_GW", as.character(week_for-4),".xlsx")
  path_for_avg <- '../../../input/dynamic_data/forecasting_method/average/'
  file_for_avg <- paste0(path_for_avg, name_for_avg)
  
  #assign(x = name_for,value = predictions_table)
  
  # Write xlsx file
  rownames(predictions_table_average) <- NULL
  write.xlsx(predictions_table_average, file_for_avg,row.names = F)
  
  
}








