##############
# Correlation team
##############

# GK_FWD
####################

correlation <- data.frame(cor = rep(0,35),p = rep(0,35))
rownames(correlation)[1:35] <- paste0("round_",4:38)

for (i in 4:38) {
  round <- paste0("round_",i)
  
  data_gk <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "GLK") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_gk)[4] <- "points"
  
  data_gk <- data_gk %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T))
  
  data_fwd <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "FWD") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_fwd)[4] <- "points"
  
  data_fwd <- data_fwd %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T))
  
  data_tot <- inner_join(data_gk,data_fwd,"Team") %>% ungroup()
  data_tot <- data_tot %>% select(3,5)
  corr <- cor(data_tot)
  correlation[i-3,1] <- corr[1,2]
  test <- cor.test(x = data_tot$mean.x,y = data_tot$mean.y)
  correlation[i-3,2] <- test$p.value
  
  if(i ==4){
    corr_data <- data_tot
  } else{
    corr_data <- rbind(corr_data,data_tot)
  }
  
}

cor.test(x = corr_data$mean.x,y = corr_data$mean.y)
#############################

# GK_MID
####################

correlation <- data.frame(cor = rep(0,35),p = rep(0,35))
rownames(correlation)[1:35] <- paste0("round_",4:38)

for (i in 4:38) {
  round <- paste0("round_",i)
  
  data_gk <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "GLK") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_gk)[4] <- "points"
  
  data_gk <- data_gk %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T))
  
  data_mid <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "MID") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_mid)[4] <- "points"
  
  data_mid <- data_mid %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T))
  
  data_tot <- inner_join(data_gk,data_mid,"Team") %>% ungroup()
  data_tot <- data_tot %>% select(3,5)
  corr <- cor(data_tot)
  correlation[i-3,1] <- corr[1,2]
  test <- cor.test(x = data_tot$mean.x,y = data_tot$mean.y)
  correlation[i-3,2] <- test$p.value
  
  if(i ==4){
    corr_data <- data_tot
  } else{
    corr_data <- rbind(corr_data,data_tot)
  }
  
}

cor.test(x = corr_data$mean.x,y = corr_data$mean.y)
#############################

# GK_DEF
####################

correlation <- data.frame(cor = rep(0,35),p = rep(0,35))
rownames(correlation)[1:35] <- paste0("round_",4:38)

for (i in 4:38) {
  round <- paste0("round_",i)
  
  data_gk <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "GLK") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_gk)[4] <- "points"
  
  data_gk <- data_gk %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T))
  
  data_def <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "DEF") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_def)[4] <- "points"
  
  data_def <- data_def %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T))
  
  data_tot <- inner_join(data_gk,data_def,"Team") %>% ungroup()
  data_tot <- data_tot %>% select(3,5)
  corr <- cor(data_tot)
  correlation[i-3,1] <- corr[1,2]
  test <- cor.test(x = data_tot$mean.x,y = data_tot$mean.y)
  correlation[i-3,2] <- test$p.value
  
  if(i ==4){
    corr_data <- data_tot
  } else{
    corr_data <- rbind(corr_data,data_tot)
  }
  
}

cor.test(x = corr_data$mean.x,y = corr_data$mean.y)
#############################

# DEF_MID
####################

correlation <- data.frame(cor = rep(0,35),p = rep(0,35))
rownames(correlation)[1:35] <- paste0("round_",4:38)

for (i in 4:38) {
  round <- paste0("round_",i)
  
  data_def <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "DEF") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_def)[4] <- "points"
  
  data_def <- data_def %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T))
  
  data_mid <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "MID") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_mid)[4] <- "points"
  
  data_mid <- data_mid %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T))
  
  data_tot <- inner_join(data_def,data_mid,"Team") %>% ungroup()
  data_tot <- data_tot %>% select(3,5)
  corr <- cor(data_tot)
  correlation[i-3,1] <- corr[1,2]
  test <- cor.test(x = data_tot$mean.x,y = data_tot$mean.y)
  correlation[i-3,2] <- test$p.value
  
  if(i ==4){
    corr_data <- data_tot
  } else{
    corr_data <- rbind(corr_data,data_tot)
  }
  
}

cor.test(x = corr_data$mean.x,y = corr_data$mean.y)
#############################

# DEF_FWD
####################

correlation <- data.frame(cor = rep(0,35),p = rep(0,35))
rownames(correlation)[1:35] <- paste0("round_",4:38)

for (i in 4:38) {
  round <- paste0("round_",i)
  
  data_def <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "DEF") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_def)[4] <- "points"
  
  data_def <- data_def %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T))
  
  data_fwd <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "FWD") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_fwd)[4] <- "points"
  
  data_fwd <- data_fwd %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T))
  
  data_tot <- inner_join(data_def,data_fwd,"Team") %>% ungroup()
  data_tot <- data_tot %>% select(3,5)
  corr <- cor(data_tot)
  correlation[i-3,1] <- corr[1,2]
  test <- cor.test(x = data_tot$mean.x,y = data_tot$mean.y)
  correlation[i-3,2] <- test$p.value
  
  if(i ==4){
    corr_data <- data_tot
  } else{
    corr_data <- rbind(corr_data,data_tot)
  }
  
}

cor.test(x = corr_data$mean.x,y = corr_data$mean.y)
#############################

# MID_FWD
####################

correlation <- data.frame(cor = rep(0,35),p = rep(0,35))
rownames(correlation)[1:35] <- paste0("round_",4:38)

for (i in 4:38) {
  round <- paste0("round_",i)
  
  data_mid <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "MID") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_mid)[4] <- "points"
  
  data_mid <- data_mid %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T))
  
  data_fwd <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "FWD") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_fwd)[4] <- "points"
  
  data_fwd <- data_fwd %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T))
  
  data_tot <- inner_join(data_mid,data_fwd,"Team") %>% ungroup()
  data_tot <- data_tot %>% select(3,5)
  corr <- cor(data_tot)
  correlation[i-3,1] <- corr[1,2]
  test <- cor.test(x = data_tot$mean.x,y = data_tot$mean.y)
  correlation[i-3,2] <- test$p.value
  
  if(i ==4){
    corr_data <- data_tot
  } else{
    corr_data <- rbind(corr_data,data_tot)
  }
  
}

cor.test(x = corr_data$mean.x,y = corr_data$mean.y)
#############################