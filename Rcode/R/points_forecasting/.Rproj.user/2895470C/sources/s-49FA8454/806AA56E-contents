#######################
# Regression for FWD that does not take into account time series of points
#######################

regressors_16_FWD <- regressors_16 %>% filter(pos == "FWD")
regressors_16_FWD$index <- as.factor(regressors_16_FWD$index)
regressors_16_FWD <- regressors_16_FWD %>% na.omit()
regressors_16_FWD <- regressors_16_FWD[,names(regressors_16_FWD) != "pos"] #equal for all
regressors_16_FWD <- regressors_16_FWD[,names(regressors_16_FWD) != "prev_3"] #Only consider total
regressors_16_FWD <- regressors_16_FWD[,names(regressors_16_FWD) != "prev_2"] #Only consider total
regressors_16_FWD <- regressors_16_FWD[,names(regressors_16_FWD) != "prev_1"] #Only consider total
regressors_16_FWD <- regressors_16_FWD[,names(regressors_16_FWD) != "index"] #Consider on group level
regressors_16_FWD <- regressors_16_FWD[,names(regressors_16_FWD) != "last_season_tot_p"] #Consider on group level
regressors_16_FWD <- regressors_16_FWD[,names(regressors_16_FWD) != "goals_con"] #Correlated with clean sheets
regressors_16_FWD <- regressors_16_FWD[,names(regressors_16_FWD) != "trans_in_prev_3"] #Only consider total
regressors_16_FWD <- regressors_16_FWD[,names(regressors_16_FWD) != "trans_in_prev_2"] #Only consider total
regressors_16_FWD <- regressors_16_FWD[,names(regressors_16_FWD) != "trans_out_prev_3"] #Only consider total
regressors_16_FWD <- regressors_16_FWD[,names(regressors_16_FWD) != "trans_out_prev_2"] #Only consider total
regressors_16_FWD <- regressors_16_FWD[,names(regressors_16_FWD) != "BPS"] #Only consider total
regressors_16_FWD <- regressors_16_FWD[,names(regressors_16_FWD) != "tot_points"] #Only consider total
regressors_16_FWD <- regressors_16_FWD[,names(regressors_16_FWD) != "dream_team"] #Only consider total
regressors_16_FWD <- regressors_16_FWD[,names(regressors_16_FWD) != "saves"] #Only consider total
regressors_16_FWD <- regressors_16_FWD[,names(regressors_16_FWD) != "pen_save"] #Only consider total
regressors_16_FWD <- regressors_16_FWD[,names(regressors_16_FWD) != "clean_sheet"] #Only consider total


#############
# Fit Model
#############
nmax = dim(regressors_16_FWD)[2]

train = seq(1,round(0.85*dim(regressors_16_FWD)[1]))

## Ridge and lasso regression
library(glmnet)
x = model.matrix(realized~.,-1,data = regressors_16_FWD)
y = regressors_16_FWD$realized

# Lasso
fit.lasso = glmnet(x,y)
plot(fit.lasso, xvar = 'dev',label = T)
cv.lasso = cv.glmnet(x[train,],y[train])
plot(cv.lasso)
coef(cv.lasso)

# Lasso train/test

lasso.tr = glmnet(x[train,],y[train])
plot(lasso.tr)
pred = predict(lasso.tr,x[-train,])
dim(pred)
rmse = sqrt(apply((y[-train]-pred)^2,2,mean))
plot(log(lasso.tr$lambda),rmse,type = "b",xlab = "Log(lambda")
lam.best = lasso.tr$lambda[order(rmse)[1]]
coef(lasso.tr,s = lam.best)

model_FWD <- lm(data = regressors_16_FWD,
                realized ~ opponent + team + cost + trans_in_prev_1 + minutes
                + goals + pen_miss + assists)
summary(model_FWD)


#Possible answer, select opponent, team, cost,trans_in_prev_1,minutes,goals,pen_miss,clean_sheet,assists
# 
######################