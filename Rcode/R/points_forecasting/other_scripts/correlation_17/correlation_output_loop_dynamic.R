#############
# Write matrix of correlation with opponontes all round round
#############
library(stringr)
library(tidyverse)
library(xlsx)
options(stringsAsFactors = F)

# Data

last_gw <- 29

for (n in 1:last_gw) {
  
  players_team_round <- players %>% select(index,PositionsList)
  team <- team_round_17 %>% select(n+1)
  players_team_round <- cbind(players_team_round,team)
  colnames(players_team_round)[3] <- "Team"
  opponent <- opponent_round_17_short %>% select(n+1)
  players_team_round <- cbind(players_team_round,opponent)
  colnames(players_team_round)[4] <- "opp"
  
  players_team_round <- players_team_round %>% mutate(tag = paste(PositionsList,Team,sep = "_")) %>% select(
    index, Team, tag, opp)
  
  players_tagged <- players_team_round
  
  cor_opp <- data.frame(matrix(nrow = 627,ncol = 627,0))
  cor_opp[1,3:627] <- players_tagged$tag
  cor_opp[3:627,1] <- players_tagged$tag
  cor_opp[2,3:627] <- players_tagged$opp
  cor_opp[3:627,2] <- players_tagged$opp
  
  folder_cor_team <- "load/data_17/data_17_output/cor_team_17/"
  file_cor_team   <- paste0(folder_cor_team,"cor_team_GW",as.character(n),".csv")
  
  cor_team <- read.csv(file = file_cor_team)
  
  cor_opp[3:627,3:627] <- cor_team
  
  ##FILL
  # GLK DEF opponents round n
  for (i in 3:627) {
    for (j in 3:627) {
      cor_opp[i,j] <- if_else(
        condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "GLK" & str_sub(cor_opp[1,j],end = 3) == "DEF" ,true = -0.106,false = as.numeric(cor_opp[i,j])
      )
    }
    
  }
  # DEF GLK opponents round n
  for (i in 3:627) {
    for (j in 3:627) {
      cor_opp[i,j] <- if_else(
        condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "DEF" & str_sub(cor_opp[1,j],end = 3) == "GLK" ,true = -0.106,false = as.numeric(cor_opp[i,j])
      )
    }
    
  }
  
  # GLK MID opponents round n
  for (i in 3:627) {
    for (j in 3:627) {
      cor_opp[i,j] <- if_else(
        condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "GLK" & str_sub(cor_opp[1,j],end = 3) == "MID" ,true = -0.312,false = as.numeric(cor_opp[i,j])
      )
    }
    
  }
  # MID GLK opponents round n
  for (i in 3:627) {
    for (j in 3:627) {
      cor_opp[i,j] <- if_else(
        condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "MID" & str_sub(cor_opp[1,j],end = 3) == "GLK" ,true = -0.312,false = as.numeric(cor_opp[i,j])
      )
    }
    
  }
  
  # GLK FWD opponents round n
  for (i in 3:627) {
    for (j in 3:627) {
      cor_opp[i,j] <- if_else(
        condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "GLK" & str_sub(cor_opp[1,j],end = 3) == "FWD" ,true = -0.336,false = as.numeric(cor_opp[i,j])
      )
    }
    
  }
  # FWD GLK opponents round n
  for (i in 3:627) {
    for (j in 3:627) {
      cor_opp[i,j] <- if_else(
        condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "MID" & str_sub(cor_opp[1,j],end = 3) == "FWD" ,true = -0.336,false = as.numeric(cor_opp[i,j])
      )
    }
    
  }
  
  # DEF DEF opponents round n
  for (i in 3:627) {
    for (j in 3:627) {
      cor_opp[i,j] <- if_else(
        condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "DEF" & str_sub(cor_opp[1,j],end = 3) == "DEF" ,true = -0.319,false = as.numeric(cor_opp[i,j])
      )
    }
    
  }
  
  # DEF MID opponents round n
  for (i in 3:627) {
    for (j in 3:627) {
      cor_opp[i,j] <- if_else(
        condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "DEF" & str_sub(cor_opp[1,j],end = 3) == "MID" ,true = -0.447,false = as.numeric(cor_opp[i,j])
      )
    }
    
  }
  # MID DEF opponents round n
  for (i in 3:627) {
    for (j in 3:627) {
      cor_opp[i,j] <- if_else(
        condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "MID" & str_sub(cor_opp[1,j],end = 3) == "DEF" ,true = -0.447,false = as.numeric(cor_opp[i,j])
      )
    }
    
  }
  
  # DEF FWD opponents round n
  for (i in 3:627) {
    for (j in 3:627) {
      cor_opp[i,j] <- if_else(
        condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "DEF" & str_sub(cor_opp[1,j],end = 3) == "FWD" ,true = -0.292,false = as.numeric(cor_opp[i,j])
      )
    }
    
  }
  # FWD DEF opponents round n
  for (i in 3:627) {
    for (j in 3:627) {
      cor_opp[i,j] <- if_else(
        condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "FWD" & str_sub(cor_opp[1,j],end = 3) == "DEF" ,true = -0.292,false = as.numeric(cor_opp[i,j])
      )
    }
    
  }
  
  # MID MID opponents round n
  for (i in 3:627) {
    for (j in 3:627) {
      cor_opp[i,j] <- if_else(
        condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "MID" & str_sub(cor_opp[1,j],end = 3) == "MID" ,true = -0.254,false = as.numeric(cor_opp[i,j])
      )
    }
    
  }
  
  # MID FWD opponents round n
  for (i in 3:627) {
    for (j in 3:627) {
      cor_opp[i,j] <- if_else(
        condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "MID" & str_sub(cor_opp[1,j],end = 3) == "FWD" ,true = -0.136,false = as.numeric(cor_opp[i,j])
      )
    }
    
  }
  # FWD MID opponents round n
  for (i in 3:627) {
    for (j in 3:627) {
      cor_opp[i,j] <- if_else(
        condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "FWD" & str_sub(cor_opp[1,j],end = 3) == "MID" ,true = -0.136,false = as.numeric(cor_opp[i,j])
      )
    }
    
  }
  
  # FWD FWD opponents round 1
  for (i in 3:627) {
    for (j in 3:627) {
      cor_opp[i,j] <- if_else(
        condition = str_sub(cor_opp[1,i],start = -3) == str_sub(cor_opp[2,j],start = -3) & str_sub(cor_opp[1,i],end = 3) == "FWD" & str_sub(cor_opp[1,j],end = 3) == "FWD" ,true = -0.119,false = as.numeric(cor_opp[i,j])
      )
    }
    
  }
  
  # Format
  cor_opp_form <- cor_opp[3:627,3:627] 
  cor_opp_form <- data.matrix(cor_opp_form)
  cor_opp_form <- data.frame(cor_opp_form)
  index <- data.frame(index = 1:625)
  cor_opp_form <- cbind(index,cor_opp_form)
  colnames(cor_opp_form)[2:626] <- 1:625
  cor_opp_form[is.na(cor_opp_form)] <- 0
  
  
  # Assign name
  name_cor <- paste0("correlation_GW", as.character(n),".xlsx")
  path_cor <- '../../../input/dynamic_data/season_17/correlation_raw_numbers/'
  file_cor <- paste0(path_cor, name_cor)
  
  # Write xlsx file
  rownames(cor_opp_form) <- NULL
  write.xlsx(cor_opp_form, file_cor,row.names = F)
  
  
  
}








