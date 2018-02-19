#Info on turnament/teams:

raw_data <- GET("https://api.sportradar.us/soccer-t3/eu/en/tournaments/sr:tournament:17/info.json?api_key=3a97ydredjbxvjghxjbzqz2g")

tournament_data <- raw_data$content %>% rawToChar() %>% fromJSON()

teams <- as.data.frame(bind_rows(tournament_data$groups[[1]]))


#info about teams/players
team_id <- teams %>% mutate(
  team_id = id
) %>% select(team_id)




url  <- "https://api.sportradar.us/soccer-t3/eu/en/teams/"
team <- team_id$team_id[1]
key  <- "/profile.json?api_key=3a97ydredjbxvjghxjbzqz2g"

call <- paste0(url,team,key)
raw_data <- GET(call)

team_data <- raw_data$content %>% rawToChar() %>% fromJSON()
players <- team_data$players %>% select(id,name,type)


for(i in 2:length(team_id$team_id)){
  call <- paste0(url,team_id$team_id[i],key)
  raw_data <- GET(call)
  team_data_temp <- raw_data$content %>% rawToChar() %>% fromJSON()
  players_temp <- team_data_temp$players %>% select(id,name,type)
  
  players <- rbind(players,players_temp)
  
  Sys.sleep(1)
}

write.csv(players, file = "input/players.csv")
library(xlsx)
write.xlsx(players, "output/players.xlsx")





