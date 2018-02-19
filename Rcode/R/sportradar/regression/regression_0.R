#############################
# The aim is to generate a generizeable regression model players contained in players_0
# for week 8
# The training set is 0-7
#############################


####################
# Fit round by round for 1 and 2
###################
options(stringsAsFactors = T)

data_3 <- rbind(players_1,players_2)

model <- lm(points ~ ., data = data_3)
summary(model)

predictions <- predict(object = model,newdata = players_2)



