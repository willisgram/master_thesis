#####################
# Function to calculate total points for 1 round
#####################


final_team_old <- function(round,selected,starting,substitutes,minutes_round){
  
  ##Decide team
  
  selected_team <- inner_join(selected, players, by = "index") %>% select(index,PositionsList)
  starting_team <- inner_join(starting, players, by = "index") %>% select(index,PositionsList)
  subs_team     <- inner_join(substitutes, players, by = "index") %>% select(index,PositionsList)
  final_team    <- starting_team
    
  minutes_played_starting <- inner_join(starting_team,minutes_round,by = "index") %>% select(
    index,PositionsList,matches(paste0("round_",as.character(round))))
  colnames(minutes_played_starting)[3] <- "min_played"
  minutes_played_selected <- inner_join(selected_team,minutes_round,by = "index") %>% select(
    index,PositionsList,matches(paste0("round_",as.character(round))))
  colnames(minutes_played_selected)[3] <- "min_played"
  minutes_played_subs     <- inner_join(subs_team,minutes_round,by = "index") %>% select(
    index,PositionsList,matches(paste0("round_",as.character(round))))
  colnames(minutes_played_subs)[3] <- "min_played"
  
  minutes_played_gk       <- minutes_played_starting %>% filter(PositionsList == "GLK")
  minutes_played_gk_sub   <- minutes_played_selected %>% filter(PositionsList == "GLK" & index != minutes_played_gk$index)
  final_team_gk           <- starting_team %>% filter(PositionsList == "GLK")
  
  #Check GK
  if(minutes_played_gk$min_played == 0 & minutes_played_gk_sub$min_played != 0){
    final_team_gk <- minutes_played_gk_sub %>% select(index,PositionsList)
  }
  
  #Check outfield
  minutes_played_starting_out <- minutes_played_starting %>% filter(PositionsList != "GLK")
  final_team_out <- minutes_played_starting_out %>% select(index,PositionsList)
  
  #Count formation
  
  d <- minutes_played_starting_out %>% filter(PositionsList == "DEF")
  d <- dim(d)[1]
  m <- minutes_played_starting_out %>% filter(PositionsList == "MID")
  m <- dim(m)[1]
  a <- minutes_played_starting_out %>% filter(PositionsList == "FWD")
  a <- dim(a)[1]
  
  ###Replace
  #Create index keepers
  not_playing_starting_index <-  which(minutes_played_starting_out$min_played == 0)
  
  #playing_subs <-  minutes_played_subs %>% filter()
  playing_subs_index <-  which(minutes_played_subs$min_played != 0)
  
  if(length(not_playing_starting_index) != 0 & length(playing_subs_index) != 0){
    for(j in 1:min(length(playing_subs_index),length(not_playing_starting_index))){
      
      if(minutes_played_subs$PositionsList[playing_subs_index[j]] == "DEF"){
        d <- d+1
      } else if(minutes_played_subs$PositionsList[playing_subs_index[j]] == "MID"){
        m <- m+1
      } else if(minutes_played_subs$PositionsList[playing_subs_index[j]] == "FWD"){
        a <- a+1
      }
      
      if(3<=d & d<=5 & 3<=m & m<=5 & 1<=a & a<=3){
      final_team_out[not_playing_starting_index[j],] <- subs_team[playing_subs_index[j],]
        }
    }
  }
 
  final_team <- rbind(final_team_gk,final_team_out) %>% arrange(
    match(PositionsList, c("GLK","DEF","MID","FWD")),index
  )
   
  return(final_team)
  
}

final_team <- function(round,selected,starting,substitutes,minutes_round){
  
  ##Decide team
  
  selected_team <- inner_join(selected, players, by = "index") %>% select(index,PositionsList)
  starting_team <- inner_join(starting, players, by = "index") %>% select(index,PositionsList)
  subs_team     <- inner_join(substitutes, players, by = "index") %>% select(index,PositionsList)
  final_team    <- starting_team
  
  minutes_played_starting <- inner_join(starting_team,minutes_round,by = "index") %>% select(
    index,PositionsList,matches(paste0("round_",as.character(round))))
  colnames(minutes_played_starting)[3] <- "min_played"
  minutes_played_selected <- inner_join(selected_team,minutes_round,by = "index") %>% select(
    index,PositionsList,matches(paste0("round_",as.character(round))))
  colnames(minutes_played_selected)[3] <- "min_played"
  minutes_played_subs     <- inner_join(subs_team,minutes_round,by = "index") %>% select(
    index,PositionsList,matches(paste0("round_",as.character(round))))
  colnames(minutes_played_subs)[3] <- "min_played"
  
  minutes_played_gk       <- minutes_played_starting %>% filter(PositionsList == "GLK")
  minutes_played_gk_sub   <- minutes_played_selected %>% filter(PositionsList == "GLK" & index != minutes_played_gk$index)
  final_team_gk           <- starting_team %>% filter(PositionsList == "GLK")
  
  #Check GK
  if(minutes_played_gk$min_played == 0 & minutes_played_gk_sub$min_played != 0){
    final_team_gk <- minutes_played_gk_sub %>% select(index,PositionsList)
  }
  
  #Check outfield
  minutes_played_starting_out <- minutes_played_starting %>% filter(PositionsList != "GLK")
  final_team_out <- minutes_played_starting_out %>% select(index,PositionsList)
  
  #Count formation
  
  d <- minutes_played_starting_out %>% filter(PositionsList == "DEF")
  d <- dim(d)[1]
  d_org <- d
  m <- minutes_played_starting_out %>% filter(PositionsList == "MID")
  m <- dim(m)[1]
  m_org <- m
  a <- minutes_played_starting_out %>% filter(PositionsList == "FWD")
  a <- dim(a)[1]
  a_org <- a
  
  ###Replace
  #Create index keepers
  not_playing_starting_index <-  which(minutes_played_starting_out$min_played == 0)
  
  #playing_subs <-  minutes_played_subs %>% filter()
  playing_subs_index <-  which(minutes_played_subs$min_played != 0)
  
  
  
  for (j in 1:length(playing_subs_index)) {
    while(length(not_playing_starting_index) != 0 & length(playing_subs_index) != 0){
      for(k in 1:length(not_playing_starting_index)){ 
        
        #Player substituted out
        
        if(minutes_played_starting_out$PositionsList[not_playing_starting_index[k]] == "DEF"){
          d <- d-1
        } else if(minutes_played_starting_out$PositionsList[not_playing_starting_index[k]] == "MID"){
          m <- m-1
        } else if(minutes_played_starting_out$PositionsList[not_playing_starting_index[k]] == "FWD"){
          a <- a-1
        }
        
        #Player substituted in 
        
        if(minutes_played_subs$PositionsList[playing_subs_index[j]] == "DEF"){
          d <- d+1
        } else if(minutes_played_subs$PositionsList[playing_subs_index[j]] == "MID"){
          m <- m+1
        } else if(minutes_played_subs$PositionsList[playing_subs_index[j]] == "FWD"){
          a <- a+1
        }
        
        if(3<=d & d<=5 & 3<=m & m<=5 & 1<=a & a<=3){
          final_team_out[not_playing_starting_index[k],] <- subs_team[playing_subs_index[j],]
          d_org <- d
          m_org <- m
          a_org <- a
          
          not_playing_starting_index <- not_playing_starting_index[-k]
          playing_subs_index         <- playing_subs_index[-j]  
          
          break
        } else{
          d <- d_org
          m <- m_org
          a <- a_org
        }
      }
    }
  }
  
  final_team <- rbind(final_team_gk,final_team_out) %>% arrange(
    match(PositionsList, c("GLK","DEF","MID","FWD")),index
  )
  
  return(final_team)
  
}

final_team_new <- function(round,selected,starting,substitutes,minutes_round){
  
  ##Decide team
  
  selected_team <- inner_join(selected, players, by = "index") %>% select(index,PositionsList)
  starting_team <- inner_join(starting, players, by = "index") %>% select(index,PositionsList)
  subs_team     <- inner_join(substitutes, players, by = "index") %>% select(index,PositionsList)
  final_team    <- starting_team
  
  minutes_played_starting <- inner_join(starting_team,minutes_round,by = "index") %>% select(
    index,PositionsList,matches(paste0("round_",as.character(round))))
  colnames(minutes_played_starting)[3] <- "min_played"
  minutes_played_selected <- inner_join(selected_team,minutes_round,by = "index") %>% select(
    index,PositionsList,matches(paste0("round_",as.character(round))))
  colnames(minutes_played_selected)[3] <- "min_played"
  minutes_played_subs     <- inner_join(subs_team,minutes_round,by = "index") %>% select(
    index,PositionsList,matches(paste0("round_",as.character(round))))
  colnames(minutes_played_subs)[3] <- "min_played"
  
  minutes_played_gk       <- minutes_played_starting %>% filter(PositionsList == "GLK")
  minutes_played_gk_sub   <- minutes_played_selected %>% filter(PositionsList == "GLK" & index != minutes_played_gk$index)
  final_team_gk           <- starting_team %>% filter(PositionsList == "GLK")
  
  #Check GK
  if(minutes_played_gk$min_played == 0 & minutes_played_gk_sub$min_played != 0){
    final_team_gk <- minutes_played_gk_sub %>% select(index,PositionsList)
  }
  
  #Check outfield
  minutes_played_starting_out <- minutes_played_starting %>% filter(PositionsList != "GLK")
  final_team_out <- minutes_played_starting_out %>% select(index,PositionsList)
  
  #Count formation
  
  d <- minutes_played_starting_out %>% filter(PositionsList == "DEF")
  d <- dim(d)[1]
  d_org <- d
  m <- minutes_played_starting_out %>% filter(PositionsList == "MID")
  m <- dim(m)[1]
  m_org <- m
  a <- minutes_played_starting_out %>% filter(PositionsList == "FWD")
  a <- dim(a)[1]
  a_org <- a
  
  ###Replace
  #Create index keepers
  not_playing_starting_index <-  which(minutes_played_starting_out$min_played == 0)
  
  #playing_subs <-  minutes_played_subs %>% filter()
  playing_subs_index <-  which(minutes_played_subs$min_played != 0)
  
  
  
  for (j in 1:length(not_playing_starting_index)) {
    while(length(not_playing_starting_index) != 0 & length(playing_subs_index) != 0){
      for(k in 1:length(playing_subs_index)){ 
        
        #Player substituted out
        
        if(minutes_played_starting_out$PositionsList[not_playing_starting_index[j]] == "DEF"){
          d <- d-1
        } else if(minutes_played_starting_out$PositionsList[not_playing_starting_index[j]] == "MID"){
          m <- m-1
        } else if(minutes_played_starting_out$PositionsList[not_playing_starting_index[j]] == "FWD"){
          a <- a-1
        }
        
        #Player substituted in 
        
        if(minutes_played_subs$PositionsList[playing_subs_index[k]] == "DEF"){
          d <- d+1
        } else if(minutes_played_subs$PositionsList[playing_subs_index[k]] == "MID"){
          m <- m+1
        } else if(minutes_played_subs$PositionsList[playing_subs_index[k]] == "FWD"){
          a <- a+1
        }
        
        if(3<=d & d<=5 & 3<=m & m<=5 & 1<=a & a<=3){
          final_team_out[not_playing_starting_index[j],] <- subs_team[playing_subs_index[k],]
          d_org <- d
          m_org <- m
          a_org <- a
          
          not_playing_starting_index <- not_playing_starting_index[-k]
          playing_subs_index         <- playing_subs_index[-j]  
          
          break
        } else{
          d <- d_org
          m <- m_org
          a <- a_org
        }
      }
    }
  }
  
  final_team <- rbind(final_team_gk,final_team_out) %>% arrange(
    match(PositionsList, c("GLK","DEF","MID","FWD")),index
  )
  
  return(final_team)
  
}

total_points <- function(round,final_team,captain,vice_captain,ill_trans,points_round,minutes_round){
  
  points_players <- inner_join(points_round,final_team,by = "index")
  points_players <- points_players %>% select(index, matches(paste0("round_",as.character(round))))
  colnames(points_players)[2] <- "points"
  
  #Captain/vice captain
  
  minutes_played_cap  <- inner_join(captain,minutes_round,by = "index") %>% select(
    index,matches(paste0("round_",as.character(round))))
  colnames(minutes_played_cap)[2] <- "min_played"
  minutes_played_vice_cap  <- inner_join(vice_captain,minutes_round,by = "index") %>% select(
    index,matches(paste0("round_",as.character(round))))
  colnames(minutes_played_vice_cap)[2] <- "min_played"
  

  if(minutes_played_cap$min_played != 0){
    
    points_players$points <- if_else(condition = points_players$index == captain$index,
                        true = points_players$points*2,
                        false = points_players$points*1)
    
    
  } else if(minutes_played_cap$min_played == 0 & minutes_played_vice_cap$min_played != 0){
    
    points_players$points <- if_else(condition = points_players$index == vice_captain$index,
                                     true = points_players$points*2,
                                     false = points_players$points*1)
    
  }
 
  total_points <- sum(points_players$points)-ill_trans*4
  
  return(total_points) 
  
}
