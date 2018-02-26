########################
#Index match
########################

path_base <- "\\\\sambaad.stud.ntnu.no/williaei/Documents/GitHub/master_thesis/output/"
path_team_0 <- paste0(path_base,"InstansXoutputGW0.txt")


players_selected <- read.table(path_team_0)

team <- c(1,23,120,199,252,261,268,296,355,394,408,521,585,603,605)

players_selected <- data.frame(index = team)

team_0 <- inner_join(players,players_selected,by = "index")


########################
# From forecast
#######################

compare <- compare %>% mutate(
  index = as.integer(index)
)

compare <- compare %>% arrange(desc(predictions))

players_selected <- head(compare,n = 20)



team_0 <- inner_join(players,players_selected,by = "index")




