##############
# Correlation opponents
##############

# GK_FWD
####################

path <- '../../../Data/BK/opponents.csv'
opponents <- read.csv2(path)
opponents <- opponents %>% mutate_all(.funs = toupper)
correlation <- data.frame(cor = rep(0,35),p = rep(0,35))
rownames(correlation)[1:35] <- paste0("round_",4:38)

for (i in 4:38) {
  round <- paste0("round_",i)

  data_gk <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "GLK") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_gk)[4] <- "points"
  
  data_gk <- data_gk %>% filter(points != 0) %>% group_by(
        PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T)) %>% mutate(key = Team)
  
  data_fwd <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "FWD") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_fwd)[4] <- "points"
  
  data_fwd <- data_fwd %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T))
  
  data_fwd <- inner_join(data_fwd,opponents,"Team") %>% select(PositionsList, Team, mean, i+3)
  colnames(data_fwd)[4] <- "key"
  
  data_tot <- inner_join(data_gk,data_fwd,"key") %>% ungroup()
  data_tot <- data_tot %>% select(3,7)
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

path <- '../../../Data/BK/opponents.csv'
opponents <- read.csv2(path)
opponents <- opponents %>% mutate_all(.funs = toupper)
correlation <- data.frame(cor = rep(0,35),p = rep(0,35))
rownames(correlation)[1:35] <- paste0("round_",4:38)

for (i in 4:38) {
  round <- paste0("round_",i)
  
  data_gk <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "GLK") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_gk)[4] <- "points"
  
  data_gk <- data_gk %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T)) %>% mutate(key = Team)
  
  data_mid <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "MID") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_mid)[4] <- "points"
  
  data_mid <- data_mid %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T))
  
  data_mid <- inner_join(data_mid,opponents,"Team") %>% select(PositionsList, Team, mean, i+3)
  colnames(data_mid)[4] <- "key"
  
  data_tot <- inner_join(data_gk,data_mid,"key") %>% ungroup()
  data_tot <- data_tot %>% select(3,7)
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

path <- '../../../Data/BK/opponents.csv'
opponents <- read.csv2(path)
opponents <- opponents %>% mutate_all(.funs = toupper)
correlation <- data.frame(cor = rep(0,35),p = rep(0,35))
rownames(correlation)[1:35] <- paste0("round_",4:38)

for (i in 4:38) {
  round <- paste0("round_",i)
  
  data_gk <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "GLK") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_gk)[4] <- "points"
  
  data_gk <- data_gk %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T)) %>% mutate(key = Team)
  
  data_def <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "DEF") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_def)[4] <- "points"
  
  data_def <- data_def %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T))
  
  data_def <- inner_join(data_def,opponents,"Team") %>% select(PositionsList, Team, mean, i+3)
  colnames(data_def)[4] <- "key"
  
  data_tot <- inner_join(data_gk,data_def,"key") %>% ungroup()
  data_tot <- data_tot %>% select(3,7)
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

# GK_GK
####################

path <- '../../../Data/BK/opponents.csv'
opponents <- read.csv2(path)
opponents <- opponents %>% mutate_all(.funs = toupper)
correlation <- data.frame(cor = rep(0,35),p = rep(0,35))
rownames(correlation)[1:35] <- paste0("round_",4:38)

for (i in 4:38) {
  round <- paste0("round_",i)
  
  data_gk <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "GLK") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_gk)[4] <- "points"
  
  data_gk <- data_gk %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T)) %>% mutate(key = Team)
  
  data_gk_2 <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "GLK") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_gk_2)[4] <- "points"
  
  data_gk_2 <- data_gk_2 %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T))
  
  data_gk_2 <- inner_join(data_gk_2,opponents,"Team") %>% select(PositionsList, Team, mean, i+3)
  colnames(data_gk_2)[4] <- "key"
  
  data_tot <- inner_join(data_gk,data_gk_2,"key") %>% ungroup()
  data_tot <- data_tot %>% select(3,7)
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

path <- '../../../Data/BK/opponents.csv'
opponents <- read.csv2(path)
opponents <- opponents %>% mutate_all(.funs = toupper)
correlation <- data.frame(cor = rep(0,35),p = rep(0,35))
rownames(correlation)[1:35] <- paste0("round_",4:38)

for (i in 4:38) {
  round <- paste0("round_",i)
  
  data_def <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "DEF") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_def)[4] <- "points"
  
  data_def <- data_def %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T)) %>% mutate(key = Team)
  
  data_fwd <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "FWD") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_fwd)[4] <- "points"
  
  data_fwd <- data_fwd %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T))
  
  data_fwd <- inner_join(data_fwd,opponents,"Team") %>% select(PositionsList, Team, mean, i+3)
  colnames(data_fwd)[4] <- "key"
  
  data_tot <- inner_join(data_def,data_fwd,"key") %>% ungroup()
  data_tot <- data_tot %>% select(3,7)
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

path <- '../../../Data/BK/opponents.csv'
opponents <- read.csv2(path)
opponents <- opponents %>% mutate_all(.funs = toupper)
correlation <- data.frame(cor = rep(0,35),p = rep(0,35))
rownames(correlation)[1:35] <- paste0("round_",4:38)

for (i in 4:38) {
  round <- paste0("round_",i)
  
  data_def <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "DEF") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_def)[4] <- "points"
  
  data_def <- data_def %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T)) %>% mutate(key = Team)
  
  data_mid <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "MID") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_mid)[4] <- "points"
  
  data_mid <- data_mid %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T))
  
  data_mid <- inner_join(data_mid,opponents,"Team") %>% select(PositionsList, Team, mean, i+3)
  colnames(data_mid)[4] <- "key"
  
  data_tot <- inner_join(data_def,data_mid,"key") %>% ungroup()
  data_tot <- data_tot %>% select(3,7)
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

# DEF_DEF
####################

path <- '../../../Data/BK/opponents.csv'
opponents <- read.csv2(path)
opponents <- opponents %>% mutate_all(.funs = toupper)
correlation <- data.frame(cor = rep(0,35),p = rep(0,35))
rownames(correlation)[1:35] <- paste0("round_",4:38)

for (i in 4:38) {
  round <- paste0("round_",i)
  
  data_def <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "DEF") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_def)[4] <- "points"
  
  data_def <- data_def %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T)) %>% mutate(key = Team)
  
  data_def_2 <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "DEF") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_def_2)[4] <- "points"
  
  data_def_2 <- data_def_2 %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T))
  
  data_def_2 <- inner_join(data_def_2,opponents,"Team") %>% select(PositionsList, Team, mean, i+3)
  colnames(data_def_2)[4] <- "key"
  
  data_tot <- inner_join(data_def,data_def_2,"key") %>% ungroup()
  data_tot <- data_tot %>% select(3,7)
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

path <- '../../../Data/BK/opponents.csv'
opponents <- read.csv2(path)
opponents <- opponents %>% mutate_all(.funs = toupper)
correlation <- data.frame(cor = rep(0,35),p = rep(0,35))
rownames(correlation)[1:35] <- paste0("round_",4:38)

for (i in 4:38) {
  round <- paste0("round_",i)
  
  data_mid <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "MID") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_mid)[4] <- "points"
  
  data_mid <- data_mid %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T)) %>% mutate(key = Team)
  
  data_fwd <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "FWD") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_fwd)[4] <- "points"
  
  data_fwd <- data_fwd %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T))
  
  data_fwd <- inner_join(data_fwd,opponents,"Team") %>% select(PositionsList, Team, mean, i+3)
  colnames(data_fwd)[4] <- "key"
  
  data_tot <- inner_join(data_mid,data_fwd,"key") %>% ungroup()
  data_tot <- data_tot %>% select(3,7)
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

# MID_MID
####################

path <- '../../../Data/BK/opponents.csv'
opponents <- read.csv2(path)
opponents <- opponents %>% mutate_all(.funs = toupper)
correlation <- data.frame(cor = rep(0,35),p = rep(0,35))
rownames(correlation)[1:35] <- paste0("round_",4:38)

for (i in 4:38) {
  round <- paste0("round_",i)
  
  data_mid <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "MID") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_mid)[4] <- "points"
  
  data_mid <- data_mid %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T)) %>% mutate(key = Team)
  
  data_mid_2 <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "MID") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_mid_2)[4] <- "points"
  
  data_mid_2 <- data_mid_2 %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T))
  
  data_mid_2 <- inner_join(data_mid_2,opponents,"Team") %>% select(PositionsList, Team, mean, i+3)
  colnames(data_mid_2)[4] <- "key"
  
  data_tot <- inner_join(data_mid,data_mid_2,"key") %>% ungroup()
  data_tot <- data_tot %>% select(3,7)
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

# FWD_FWD
####################

path <- '../../../Data/BK/opponents.csv'
opponents <- read.csv2(path)
opponents <- opponents %>% mutate_all(.funs = toupper)
correlation <- data.frame(cor = rep(0,35),p = rep(0,35))
rownames(correlation)[1:35] <- paste0("round_",4:38)

for (i in 4:38) {
  round <- paste0("round_",i)
  
  data_fwd <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "FWD") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_fwd)[4] <- "points"
  
  data_fwd <- data_fwd %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T)) %>% mutate(key = Team)
  
  data_fwd_2 <- inner_join(players,points_round_16,"index")%>% filter(
    PositionsList == "FWD") %>% select(
      index,PositionsList,Team,matches(round))
  colnames(data_fwd_2)[4] <- "points"
  
  data_fwd_2 <- data_fwd_2 %>% filter(points != 0) %>% group_by(
    PositionsList, Team) %>% summarise(mean = mean(points,na.rm = T))
  
  data_fwd_2 <- inner_join(data_fwd_2,opponents,"Team") %>% select(PositionsList, Team, mean, i+3)
  colnames(data_fwd_2)[4] <- "key"
  
  data_tot <- inner_join(data_fwd,data_fwd_2,"key") %>% ungroup()
  data_tot <- data_tot %>% select(3,7)
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






