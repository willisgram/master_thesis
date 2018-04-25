#####################
# Loop to generate cost sheets
#####################
library(xlsx)
# index - gw_1 - gw_2 ...


# Ready to use, only folder missing

last_gw <- 35

#given input
h <- 11

for(week_for in 1:last_gw){
  
  cost_table <- cost_round_17 %>% select(index)
  cost_table[2] <- cost_round_17[week_for+1]
  cost_table <- cost_table %>% arrange(index)
  cost_table[is.na(cost_table)] <- 100000000
  
  cost_table[,3:(h+1)] <- cost_table[,2]
  colnames(cost_table)[2:(h+1)] <- paste0("GW",(week_for):(week_for+h-1))
  
  #Costs
  name_cost <- paste0("player_cost_GW", as.character(week_for),".xlsx")
  path_cost <- '../../../input/static_data/cost/season_17/'
  file_cost <- paste0(path_cost, name_cost)
  
  #assign(x = name_for,value = predictions_table)
  
  
  # Write xlsx file
  rownames(cost_table) <- NULL
  write.xlsx(cost_table, file_cost,row.names = F)
  
}
