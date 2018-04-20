########################
# Caculcate points for all rounds
#########################
library(tidyverse)

round_start <- 1
round_stop  <- 5

total_points_round <- data.frame(round = round_start:round_stop,
                                 cost = rep(0,round_stop-round_start+1),
                                 rem_bud = rep(0,round_stop-round_start+1),
                                 points = rep(0,round_stop-round_start+1),
                                 points_brutto = rep(0,round_stop-round_start+1),
                                 chip = rep(0,round_stop-round_start+1),
                                 ill_trans = rep(0,round_stop-round_start+1),
                                 ill_trans_test = rep(0,round_stop-round_start+1))

final_team_each_round <- data.frame()


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
  
  #Illegal transfers
  if(i != 1){
    file_ill_trans <- paste0(path,method,folder,"number_illegal_transfers.csv")
    ill_trans <- read.csv(file_ill_trans,header = F)
    colnames(ill_trans)[1] <- "ill_trans"
    ill_trans_round <- ill_trans %>% mutate(index = as.integer(ill_trans)) %>% select(index)  
  } else{
    ill_trans_round <- 0
  }
  
  
  #Remaining
  file_rem <- paste0(path,method,folder,"remaining_budget.csv")
  rem_bud <- read.csv(file_rem,header = F)
  colnames(rem_bud)[1] <- "remaining_budget"
  
  #Gamechips
  #############
  # Triple Captain
  file_triple_captain <- paste0(path,method,folder,"triple_captain.csv")
  triple_captain <- read.csv(file_triple_captain,header = F)
  colnames(triple_captain)[1] <- "triple_captain"
  triple_captain_round <- triple_captain %>% mutate(index = as.integer(triple_captain)) %>% select(index)
  
  # Wildcard
  file_wildcard <- paste0(path,method,folder,"wildcard.csv")
  wildcard <- read.csv(file_wildcard,header = F)
  colnames(wildcard)[1] <- "wildcard"
  wildcard_round <- wildcard %>% mutate(index = as.integer(wildcard)) %>% select(index)
  
  # Free Hit
  file_free_hit <- paste0(path,method,folder,"free_hit.csv")
  free_hit <- read.csv(file_free_hit,header = F)
  colnames(free_hit)[1] <- "free_hit"
  free_hit_round <- free_hit %>% mutate(index = as.integer(free_hit)) %>% select(index)
  
  # Bench boost
  file_bench_boost <- paste0(path,method,folder,"bench_boost.csv")
  bench_boost <- read.csv(file_bench_boost,header = F)
  colnames(bench_boost)[1] <- "bench_boost"
  bench_boost_round <- bench_boost %>% mutate(index = as.integer(bench_boost)) %>% select(index)
  #############
  if(bench_boost_round != 0){
    final_team_round <- inner_join(selected_round, players, by = "index") %>% select(index,PositionsList)
    points_player <- total_points_chips_player(round = round,final_team = final_team_round,
                                               captain = captain_round,
                                               vice_captain = vice_captain_round,
                                               triple_captain = triple_captain_round,
                                               penalty = penalty_round,points_round = points_round_17,
                                               minutes_round = minutes_round_17)
    final_team_round_temp <- inner_join(final_team_round,points_player,"index")
    final_team_each_round <- rbind(final_team_each_round,final_team_round_temp)
    
  } else{
    final_team_round <- final_team_new(round = round,selected = selected_round,starting = starting_round,
                                       substitutes = substitutes_round,minutes_round = minutes_round_17)
    points_player <- total_points_chips_player(round = round,final_team = final_team_round,
                                               captain = captain_round,
                                               vice_captain = vice_captain_round,
                                               triple_captain = triple_captain_round,
                                               penalty = penalty_round,points_round = points_round_17,
                                               minutes_round = minutes_round_17)
    final_team_round_temp <- inner_join(players,final_team_round,by = "index")
    final_team_round_temp <- inner_join(final_team_round_temp,points_player,"index")
    final_team_each_round <- rbind(final_team_each_round,final_team_round_temp)
  }

  
  penalty_round <- penalty(ill_trans = ill_trans_round,wildcard = wildcard_round,free_hit = free_hit_round )
  
  total_points_round$points[i]  <- total_points_chips(round = round,final_team = final_team_round,
                                                captain = captain_round,
                                                vice_captain = vice_captain_round,triple_captain = triple_captain_round,penalty = penalty_round,
                                                points_round = points_round_17,minutes_round = minutes_round_17)
  
  total_points_round$points_brutto[i]  <- total_points_chips(round = round,final_team = final_team_round,
                                                       captain = captain_round,
                                                       vice_captain = vice_captain_round,
                                                       triple_captain = triple_captain_round,
                                                       penalty = 0,
                                                       points_round = points_round_17,minutes_round = minutes_round_17)
  
  cost_team <- inner_join(selected_round,cost_round_17,"index") %>% select(i+1)
  total_points_round$cost[i] <- sum(cost_team)
  total_points_round$rem_bud[i] <- rem_bud$remaining_budget
  total_points_round$ill_trans[i] <- ill_trans_round
  
  #ill trans test
  if(i != 1){
    
    total_points_round$ill_trans_test[i] <- 14 - dim(inner_join(selected_round,selected_prev,'index'))[1]
  }
  
  
  
  if(wildcard_round != 0 | bench_boost_round != 0 | free_hit_round != 0 | triple_captain_round != 0){
    total_points_round$chip[i] <- 1
  }
 
  selected_prev <- selected_round
   
}

total_points_round$points <- as.numeric(total_points_round$points)
