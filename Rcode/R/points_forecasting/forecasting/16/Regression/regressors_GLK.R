#######################
# Regression for GLK that does not take into account time series of points, nor direct influencing data except tot_points
#######################

regressors_16_GLK <- regressors_16 %>% filter(pos == "GLK")
regressors_16_GLK$index <- as.factor(regressors_16_GLK$index)
regressors_16_GLK <- regressors_16_GLK %>% na.omit()
regressors_16_GLK <- regressors_16_GLK[,names(regressors_16_GLK) != "pos"] #equal for all
regressors_16_GLK <- regressors_16_GLK[,names(regressors_16_GLK) != "prev_3"] #Only consider total
regressors_16_GLK <- regressors_16_GLK[,names(regressors_16_GLK) != "prev_2"] #Only consider total
regressors_16_GLK <- regressors_16_GLK[,names(regressors_16_GLK) != "prev_1"] #Only consider total
regressors_16_GLK <- regressors_16_GLK[,names(regressors_16_GLK) != "index"] #Consider on group level
regressors_16_GLK <- regressors_16_GLK[,names(regressors_16_GLK) != "last_season_tot_p"] #Consider on group level
regressors_16_GLK <- regressors_16_GLK[,names(regressors_16_GLK) != "goals_con"] #Correlated with clean sheets
regressors_16_GLK <- regressors_16_GLK[,names(regressors_16_GLK) != "trans_in_prev_3"] #Only consider total
regressors_16_GLK <- regressors_16_GLK[,names(regressors_16_GLK) != "trans_in_prev_2"] #Only consider total
regressors_16_GLK <- regressors_16_GLK[,names(regressors_16_GLK) != "trans_out_prev_3"] #Only consider total
regressors_16_GLK <- regressors_16_GLK[,names(regressors_16_GLK) != "trans_out_prev_2"] #Only consider total
regressors_16_GLK <- regressors_16_GLK[,names(regressors_16_GLK) != "BPS"] #Only consider total
regressors_16_GLK <- regressors_16_GLK[,names(regressors_16_GLK) != "tot_points"] #Only consider total
regressors_16_GLK <- regressors_16_GLK[,names(regressors_16_GLK) != "dream_team"] #Only consider total

#############
# Fit Model
#############
nmax = dim(regressors_16_GLK)[2]

train = seq(1,round(0.85*dim(regressors_16_GLK)[1]))

## Ridge and lasso regression
library(glmnet)
x = model.matrix(realized~.,-1,data = regressors_16_GLK)
y = regressors_16_GLK$realized

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

model_GLK <- lm(data = regressors_16_GLK,
                realized ~ opponent + team + cost + trans_in_prev_1 + trans_out_prev_1 + H_A + minutes
                + saves + assists + own_goals)
summary(model_DEF)

#Possible answer, select opponent, team, cost,trans_in_prev_1,trans_out_prev_1,H_A,minutes,saves,
# assists, own goals
######################