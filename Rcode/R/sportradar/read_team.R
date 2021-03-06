########################
# Read csv from Mosel
#########################


options(stringsAsFactors = F)

total_points <- 0
tot_points_round <- data.frame()

for (i in 6:6) {
  
  path <- "../../../output/forecasting_method/"
  method <- "regression/"
  folder <- paste0("GW",i,"/")
  
  #Captain
  file_cap <- paste0(path,method,folder,"captain.csv")
  captain <- read.csv(file_cap,header = F)
  colnames(captain)[1] <- "captain"
  name_captain <- paste0("captain_",as.character(i+4))
  captain_round <- captain %>% mutate(index = as.integer(captain)) %>% select(index)
  assign(name_captain,captain_round)
  
  #Vice captain
  #Starting
  file_vice <- paste0(path,method,folder,"vice_captain.csv")
  vice_captain <- read.csv(file_vice,header = F)
  colnames(vice_captain)[1] <- "vice_captain"
  name_vice_captain <- paste0("vice_captain_",as.character(i+4))
  vice_captain_round <- vice_captain %>% mutate(index = as.integer(vice_captain)) %>% select(index)
  assign(name_vice_captain,vice_captain_round)
  
  #Remaining budget
  file_bud <- paste0(path,method,folder,"remaining_budget.csv")
  budget <- read.csv(file_bud,header = F)
  
  #Selected
  file_sel <- paste0(path,method,folder,"selected.csv")
  selected <- read.csv(file_sel,header = F)
  colnames(selected)[1] <- "selected"
  name_selected <- paste0("selected_",as.character(i+4))
  selected_round <- selected %>% mutate(index = as.integer(selected)) %>% select(index)
  assign(name_selected,selected_round)
  
  #Starting
  file_start <- paste0(path,method,folder,"starting.csv")
  starting <- read.csv(file_start,header = F)
  colnames(starting)[1] <- "starting"
  name_starting <- paste0("starting_",as.character(i+4))
  starting_round <- starting %>% mutate(index = as.integer(starting)) %>% select(index)
  #starting_round <- inner_join(points_round_16,starting_round,by = "index")
  #starting_round <- starting_round %>% select(index, matches(paste0("round_",as.character(i+4))))
  assign(name_starting,starting_round)
  
  #Substitutes
  file_sub <- paste0(path,method,folder,"substitutes.csv")
  substitutes <- read.csv(file_sub,header = F)
  colnames(substitutes)[1] <- "substitutes"
  name_substitutes <- paste0("substitutes_",as.character(i+4))
  substitutes_round <- substitutes %>% mutate(index = as.integer(substitutes)) %>% select(index)
  assign(name_substitutes,substitutes_round)
  
  
}

rm(list = c("selected","starting","table"))
  
#####
# Evaluate
#####

points_total <- sum(starting_5$round_5) + sum(starting_6$round_6) + sum(starting_7$round_7) + sum(starting_8$round_8) + sum(starting_9$round_9) 













  
