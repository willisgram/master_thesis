#####################
# Loop to generate cost sheets
#####################
library(xlsx)
# index - gw_1 - gw_2 ...

# Suggestion, only change opponent and team
# Not using forecast as new previous



#given input
k <- 3
h <- 5

for(week_for in 5:7){
  
  cost_table <- cost_round_16 %>% select(index)
  cost_table[2] <- cost_round_16[i+1]
  cost_table <- cost_table %>% arrange(index)
  
  cost_table[,3:(h+1)] <- cost_table[,2]
  colnames(cost_table)[3:(h+1)] <- paste0("GW_",(week_for+1):(week_for+h-1))
  
  #Costs
  name_cost <- paste0("player_cost_GW", as.character(week_for),".xlsx")
  path_cost <- '../../../input/static_data/cost/'
  file_cost <- paste0(path_cost, name_cost)
  
  #assign(x = name_for,value = predictions_table)
  
  
  # Write xlsx file
  rownames(cost_table) <- NULL
  write.xlsx(cost_table, file,row.names = F)
  
}
