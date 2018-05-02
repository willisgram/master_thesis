###################
#Betradar import
###################
library(tidyverse)
library(httr)
library(jsonlite)
library(data.table)
options(stringsAsFactors = FALSE)

# address

url <- "https://stgapi.betradar.com/v1/sports/en/"

match_id <- "tournaments/sr:season:40942/"
end_key  <- "info.xml?api_key=bhUEKNLMRXenlIEl5b"

#raw_data <- GET("https://api.sportradar.us/soccer-xt3/eu/en/matches/sr:match:11830276/summary.json?api_key=3a97ydredjbxvjghxjbzqz2g")
call <- paste0(url,match_id,end_key)
raw_data <- GET(call)
raw_data$status_code

GET("https://stgapi.betradar.com/v1/sports/en/tournaments/sr:season:40942/info.xml")

#Formatting
stats <- raw_data$content %>% rawToChar() %>% fromJSON()