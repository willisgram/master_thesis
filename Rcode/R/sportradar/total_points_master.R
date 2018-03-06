########################
# Caculcate points for all rounds
#########################

total_points_round <- data.frame(round = 8:15,points = rep(0,8))

for (i in 4:11) {
  
  path <- "../../../output/forecasting_method/"
  method <- "regression/"
  folder <- paste0("GW",i,"/")
  round  <- i+4
  
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
  
  final_team_round <- final_team(round = round,selected = selected_round,starting = starting_round,
                          substitutes = substitutes_round,minutes_round = minutes_round_16)
  if(i == 4){
    ill_trans_round <- 0
  } else{
    ill_trans_round  <- max(0,10 - dim(inner_join(final_team_round,final_team_prev,'index'))[1])
  }
  
  total_points_round$points[i-3]  <- total_points(round = round,final_team = final_team_round,
                                      captain = captain_round,
                                      vice_captain = vice_captain_round,ill_trans = ill_trans_round,
                                      points_round = points_round_16,minutes_round = minutes_round_16)
 
  final_team_prev <- final_team_round 
  
}
