###################
#sportradar import
###################

library(tidyverse)
library(httr)
library(jsonlite)
options(stringsAsFactors = FALSE)

#Retrive summary
url <- "https://api.sportradar.us/soccer-xt3/eu/en/matches/"
match_id <- "sr:match:11830620"
end_key  <- "/summary.json?api_key=3a97ydredjbxvjghxjbzqz2g"

#raw_data <- GET("https://api.sportradar.us/soccer-xt3/eu/en/matches/sr:match:11830276/summary.json?api_key=3a97ydredjbxvjghxjbzqz2g")
call <- paste0(url,match_id,end_key)
raw_data <- GET(call)

#Formatting
stats <- raw_data$content %>% rawToChar() %>% fromJSON()
players_home <- stats$statistics$teams$players[[1]]
players_away <- stats$statistics$teams$players[[2]]


#Create column Surname
players_home <- players_home %>% 
  mutate(Surname = substr(players_home$name,start = 1,stop = (regexpr(pattern = ",",text = players_home$name))-1)
)

players_away <- players_away %>% 
  mutate(Surname = name,
         Surname = substr(players_away$name,start = 1,stop = (regexpr(pattern = ",",text = players_away$name))-1)
  )

#Need to hanlde players with "wrong" format from overlor

players_away$Surname <- case_when(players_away$Surname == "" ~ players_away$name,
                                  TRUE ~ as.character(players_away$Surname))



