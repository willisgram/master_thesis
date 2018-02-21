#################
#BK average
################



points_round <- points_round %>% na.omit()


points_round <- points_round %>% mutate(
  average_5_7 = (round_5 + round_6 + round_7)/3
)


