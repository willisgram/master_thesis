########################
# Read txt from Mosel
#########################




library(readr)
options(stringsAsFactors = F)

for (i in 1:5) {
  
  file_name <- paste0("../../../output/forecasting_method/regression/regression_output_GW",i,".txt")
  
  table <- read.table(file = file_name, header = T, skip = 1)

  selected <- data.frame( selected = table[1:15,1])
  starting <- data.frame( starting = table[17:27,1])
  
  name_starting <- paste0("starting_",as.character(i+4))
  #name_selected <- paste0("selected_",as.character(i+4))
  players_starting <- starting %>% mutate(index = as.integer(starting)) %>% select(index)
  team <- inner_join(points_round_16,players_starting,by = "index")
  
  
  
  assign(name_starting,team)
  #assign(name_selected,selected)
  
}

rm(list = c("selected","starting","table"))
  
#####
# Evaluate
#####

points_total <- sum(starting_5$round_5) + sum(starting_6$round_6) + sum(starting_7$round_7) + sum(starting_8$round_8) + sum(starting_9$round_9) 













  
