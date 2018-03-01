########################
# Read txt from Mosel
#########################




library(readr)
options(stringsAsFactors = F)

total_points <- 0
tot_points_round <- data.frame()

for (i in 1:10) {
  
  path <- "../../../output/forecasting_method/"
  method <- "regression/"
  folder <- paste0("GW",i,"/")
  
  #Captain
  file_cap <- paste0(path,method,folder,"captain.csv")
  captain <- read.csv(file_cap,header = F)
  
  #Remaining budget
  file_bud <- paste0(path,method,folder,"remaining_budget.csv")
  budget <- read.csv(file_bud,header = F)
  
  #Selected
  file_sel <- paste0(path,method,folder,"selected.csv")
  selected <- read.csv(file_sel,header = F)
  
  #Starting
  file_start <- paste0(path,method,folder,"starting.csv")
  starting <- read.csv(file_start,header = F)
  colnames(starting)[1] <- "starting"
  name_starting <- paste0("starting_",as.character(i+4))
  starting_round <- starting %>% mutate(index = as.integer(starting)) %>% select(index)
  starting_round <- inner_join(points_round_16,starting_round,by = "index")
  starting_round <- starting_round %>% select(index, matches(paste0("round_",as.character(i+4))))
  assign(name_starting,starting_round)
  
  name_tot_points <- paste0("total_points_round_", as.character(i+4))
  tot_points_round[1,i] <- sum(starting_round[,2])
  colnames(tot_points_round)[i] <- paste0("round_",i+4)
  total_points <- total_points + sum(starting_round[,2])
  
  #Substitutes
  file_sub <- paste0(path,method,folder,"substitutes.csv")
  substitutes <- read.csv(file_sub,header = F)
  
  #Vice captain
  #Starting
  file_vice <- paste0(path,method,folder,"vice_captain.csv")
  vice_captain <- read.csv(file_vice,header = F)
  
  
  #name_selected <- paste0("selected_",as.character(i+4))
  #players_starting <- starting %>% mutate(index = as.integer(starting)) %>% select(index)
  #team <- inner_join(points_round_16,players_starting,by = "index")
  
  
  
  #assign(name_starting,team)
  #assign(name_selected,selected)
  
}

rm(list = c("selected","starting","table"))
  
#####
# Evaluate
#####

points_total <- sum(starting_5$round_5) + sum(starting_6$round_6) + sum(starting_7$round_7) + sum(starting_8$round_8) + sum(starting_9$round_9) 













  
