#######################
# Regression for DEF that does not take into account time series of points
#######################

regressors_16_DEF <- regressors_16 %>% filter(pos == "DEF")
regressors_16_DEF$index <- as.factor(regressors_16_DEF$index)
regressors_16_DEF <- regressors_16_DEF %>% na.omit()
regressors_16_DEF <- regressors_16_DEF[,names(regressors_16_DEF) != "pos"] #equal for all
regressors_16_DEF <- regressors_16_DEF[,names(regressors_16_DEF) != "prev_3"] #Only consider total
regressors_16_DEF <- regressors_16_DEF[,names(regressors_16_DEF) != "prev_2"] #Only consider total
regressors_16_DEF <- regressors_16_DEF[,names(regressors_16_DEF) != "prev_1"] #Only consider total
regressors_16_DEF <- regressors_16_DEF[,names(regressors_16_DEF) != "index"] #Consider on group level
regressors_16_DEF <- regressors_16_DEF[,names(regressors_16_DEF) != "last_season_tot_p"] #Consider on group level
regressors_16_DEF <- regressors_16_DEF[,names(regressors_16_DEF) != "goals_con"] #Correlated with clean sheets
regressors_16_DEF <- regressors_16_DEF[,names(regressors_16_DEF) != "trans_in_prev_3"] #Only consider total
regressors_16_DEF <- regressors_16_DEF[,names(regressors_16_DEF) != "trans_in_prev_2"] #Only consider total
regressors_16_DEF <- regressors_16_DEF[,names(regressors_16_DEF) != "trans_out_prev_3"] #Only consider total
regressors_16_DEF <- regressors_16_DEF[,names(regressors_16_DEF) != "trans_out_prev_2"] #Only consider total
regressors_16_DEF <- regressors_16_DEF[,names(regressors_16_DEF) != "BPS"] #Only consider total
regressors_16_DEF <- regressors_16_DEF[,names(regressors_16_DEF) != "tot_points"] #Only consider total
regressors_16_DEF <- regressors_16_DEF[,names(regressors_16_DEF) != "dream_team"] #Only consider total
regressors_16_DEF <- regressors_16_DEF[,names(regressors_16_DEF) != "saves"] #Only consider total
regressors_16_DEF <- regressors_16_DEF[,names(regressors_16_DEF) != "pen_save"] #Only consider total

#############
# Fit Model
#############
nmax = dim(regressors_16_DEF)[2]

train = seq(1,round(0.85*dim(regressors_16_DEF)[1]))

## Ridge and lasso regression
library(glmnet)
x = model.matrix(realized~.,-1,data = regressors_16_DEF)
y = regressors_16_DEF$realized

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
# clean_sheet
######################