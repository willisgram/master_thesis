############
# Script to test results
############


points_selected_10 <- inner_join(selected_round,points_round_17,"index") %>% select(index,i+1)
points_starting_10 <- inner_join(starting_round,points_round_17,"index") %>% select(index,i+1)
minutes_starting_10 <- inner_join(starting_round,minutes_round_17,"index") %>% select(index,i+1)
minutes_selected_10 <- inner_join(selected_round,minutes_round_17,"index") %>% select(index,i+1)
points_final_team_10 <- inner_join(final_team_round,points_round_17,"index") %>% select(index,2,i+2)
captain_10 <- captain_round
vice_captain_10 <- vice_captain_round

points_selected_11 <- inner_join(selected_round,points_round_17,"index") %>% select(index,i+1)
points_starting_11 <- inner_join(starting_round,points_round_17,"index") %>% select(index,i+1)
minutes_starting_11 <- inner_join(starting_round,minutes_round_17,"index") %>% select(index,i+1)
minutes_selected_11 <- inner_join(selected_round,minutes_round_17,"index") %>% select(index,i+1)
points_final_team_11 <- inner_join(final_team_round,points_round_17,"index") %>% select(index,2,i+2)
captain_11 <- captain_round
vice_captain_11 <- vice_captain_round