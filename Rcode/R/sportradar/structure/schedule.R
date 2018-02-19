#Scedule


raw_data <- GET("https://api.sportradar.us/soccer-xt3/eu/en/tournaments/sr:tournament:17/schedule.json?api_key=3a97ydredjbxvjghxjbzqz2g")

schedule_data <- raw_data$content %>% rawToChar() %>% fromJSON()

schedule_data <- schedule_data$sport_events %>% select(id,tournament_round,competitors)

matches <- as.data.frame(bind_rows(schedule_data$competitors))
round   <- as.data.frame(bind_rows(schedule_data$tournament_round))

home_teams <- matches %>% filter(qualifier == "home") %>% mutate(home_team = abbreviation) %>% select(home_team)
away_teams <- matches %>% filter(qualifier == "away") %>% mutate(away_team = abbreviation) %>% select(away_team)
match_id   <- schedule_data %>% select(id)
round      <- round %>% mutate(round_number = number) %>% select(round_number)

schedule <- bind_cols(match_id,home_teams,away_teams,round)


