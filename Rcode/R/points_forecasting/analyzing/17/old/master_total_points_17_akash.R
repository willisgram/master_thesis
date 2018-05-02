########################
# Caculcate points for all rounds
#########################'
#########################'
#load: players, points, cost, minutes 
#total_points_function: final_team_new, total_points

round_start <- 1
round_stop  <- 27

total_points_round <- data.frame(round = round_start:round_stop,
                                 cost = rep(0,round_stop-round_start+1),
                                 rem_bud = rep(0,round_stop-round_start+1),
                                 points = rep(0,round_stop-round_start+1),
                                 points_brutto = rep(0,round_stop-round_start+1))

for (i in round_start:round_stop) {
  
  path <- "../../../output/season_17/forecasting_method/"
  method <- "average/"
  folder <- paste0("GW",i,"/")
  round  <- i
  
  #Captain
  file_cap <- paste0(path,method,folder,"captain.csv")
  captain <- read.csv(file_cap,header = F)
  colnames(captain)[1] <- "captain"
  captain_round <- captain %>% mutate(index = as.integer(captain)) %>% select(index)
  
  #Vice captain
  file_vice <- paste0(path,method,folder,"vice_captain.csv")
  vice_captain <- read.csv(file_vice,header = F)
  colnames(vice_captain)[1] <- "vice_captain"
  vice_captain_round <- vice_captain %>% mutate(index = as.integer(vice_captain)) %>% select(index)
  
  #Selected
  file_sel <- paste0(path,method,folder,"selected.csv")
  selected <- read.csv(file_sel,header = F)
  colnames(selected)[1] <- "selected"
  selected_round <- selected %>% mutate(index = as.integer(selected)) %>% select(index)
  
  #Starting
  file_start <- paste0(path,method,folder,"starting.csv")
  starting <- read.csv(file_start,header = F)
  colnames(starting)[1] <- "starting"
  starting_round <- starting %>% mutate(index = as.integer(starting)) %>% select(index)
  
  #Substitutes
  file_sub <- paste0(path,method,folder,"substitutes.csv")
  substitutes <- read.csv(file_sub,header = F)
  colnames(substitutes)[1] <- "substitutes"
  substitutes_round <- substitutes %>% mutate(index = as.integer(substitutes)) %>% select(index)
  
  #Remaining
  file_rem <- paste0(path,method,folder,"remaining_budget.csv")
  rem_bud <- read.csv(file_rem,header = F)
  colnames(rem_bud)[1] <- "remaining_budget"
  
  final_team_round <- final_team_new(round = round,selected = selected_round,starting = starting_round,
                                     substitutes = substitutes_round,minutes_round = minutes_round_17)
  if(i == 1){
    ill_trans_round <- 0
  } else{
    ill_trans_round  <- max(0,14 - dim(inner_join(selected_round,selected_prev,'index'))[1])
  }
  
  total_points_round$points[i]  <- total_points(round = round,final_team = final_team_round,
                                                captain = captain_round,
                                                vice_captain = vice_captain_round,ill_trans = ill_trans_round,
                                                points_round = points_round_17,minutes_round = minutes_round_17)
  
  total_points_round$points_brutto[i]  <- total_points(round = round,final_team = final_team_round,
                                                       captain = captain_round,
                                                       vice_captain = vice_captain_round,ill_trans = 0,
                                                       points_round = points_round_17,minutes_round = minutes_round_17)
  
  cost_team <- inner_join(selected_round,cost_round_17,"index") %>% select(i+1)
  total_points_round$cost[i] <- sum(cost_team)
  
  total_points_round$rem_bud[i] <- rem_bud$remaining_budget
  
  selected_prev <- selected_round
  
}
