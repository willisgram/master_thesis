#######################
# Regression for MID that does not take into account time series of points
#######################

regressors_16_MID <- regressors_16 %>% filter(pos == "MID")
regressors_16_MID$index <- as.factor(regressors_16_MID$index)
regressors_16_MID <- regressors_16_MID %>% na.omit()
regressors_16_MID <- regressors_16_MID[,names(regressors_16_MID) != "pos"] #equal for all
regressors_16_MID <- regressors_16_MID[,names(regressors_16_MID) != "prev_3"] #Only consider total
regressors_16_MID <- regressors_16_MID[,names(regressors_16_MID) != "prev_2"] #Only consider total
regressors_16_MID <- regressors_16_MID[,names(regressors_16_MID) != "prev_1"] #Only consider total
regressors_16_MID <- regressors_16_MID[,names(regressors_16_MID) != "index"] #Consider on group level
regressors_16_MID <- regressors_16_MID[,names(regressors_16_MID) != "last_season_tot_p"] #Consider on group level
regressors_16_MID <- regressors_16_MID[,names(regressors_16_MID) != "goals_con"] #Correlated with clean sheets
regressors_16_MID <- regressors_16_MID[,names(regressors_16_MID) != "trans_in_prev_3"] #Only consider total
regressors_16_MID <- regressors_16_MID[,names(regressors_16_MID) != "trans_in_prev_2"] #Only consider total
regressors_16_MID <- regressors_16_MID[,names(regressors_16_MID) != "trans_out_prev_3"] #Only consider total
regressors_16_MID <- regressors_16_MID[,names(regressors_16_MID) != "trans_out_prev_2"] #Only consider total
regressors_16_MID <- regressors_16_MID[,names(regressors_16_MID) != "BPS"] #Only consider total
regressors_16_MID <- regressors_16_MID[,names(regressors_16_MID) != "tot_points"] #Only consider total
regressors_16_MID <- regressors_16_MID[,names(regressors_16_MID) != "dream_team"] #Only consider total
regressors_16_MID <- regressors_16_MID[,names(regressors_16_MID) != "saves"] #Only consider total
regressors_16_MID <- regressors_16_MID[,names(regressors_16_MID) != "pen_save"] #Only consider total


#############
# Fit Model
#############
nmax = dim(regressors_16_MID)[2]

train = seq(1,round(0.5*dim(regressors_16_MID)[1]))

## Ridge and lasso regression
library(glmnet)
x = model.matrix(realized~.,-1,data = regressors_16_MID)
y = regressors_16_MID$realized

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


#Possible answer, select opponent, team, cost,trans_in_prev_1,trans_out_prev_1,H_A,minutes,y_cards,
# goals,pen_miss ,clean_sheet, assists, r_cards
######################