#####################
# Master script to fit regression model to round 1:(38-k) for the  2016 season
#####################
library(tidyverse)

# Suggestion, only change opponent and team
# Not using forecast as new previous

#given input
k <- 5 #(uses 5 previous matches)
# week_for <- 15

options(stringsAsFactors = F)
  
# Distribute points from k matches
######################
# Distribute points from k matches
  
  
  options(stringsAsFactors = F)
  #2 <- week_for%%k+4
  upper_lim <- 39-2*k+2 #+1 due to index
  
  #points
  for (i in seq(from = 2,by = 1, to = upper_lim)) {
    
    points_round_temp <- data.frame(index = 1:625)
    points_round_temp[,2:(k+1)] <- points_round_16[,i:(i+(k-1))]
    
    colnames(points_round_temp)[2:(k)] <- paste0("prev_",(k-1):1)
    colnames(points_round_temp)[(k+1)] <- "realized"
    
    if(i == 2){
      points_round_k_16 <- points_round_temp
    } else{
      points_round_k_16 <- rbind(points_round_k_16,points_round_temp)  
    }
    
  }
  
  #Opponents
  for (i in seq(from = 2,by = 1, to = upper_lim)) {
    
    opponent_round_temp <- data.frame(index = 1:625)
    opponent_round_temp[,2] <- opponent_round_16[,(i+k-1)]
    
    colnames(opponent_round_temp)[2] <- "opponent"
    
    if(i == 2){
      opponent_round_k_16 <- opponent_round_temp
    } else{
      opponent_round_k_16    <- rbind(opponent_round_k_16,opponent_round_temp)  
    }
    
  }
  
  opponent_round_k_16 <- opponent_round_k_16 %>% select(opponent)
  opponent_round_k_16$opponent <- if_else(
    opponent_round_k_16$opponent == "Hull City",true = "Huddersfield",false = opponent_round_k_16$opponent)
  opponent_round_k_16$opponent <- if_else(
    opponent_round_k_16$opponent == "Middlesbrough",true = "Brighton",false = opponent_round_k_16$opponent) 
  opponent_round_k_16$opponent <- if_else(
    opponent_round_k_16$opponent == "Sunderland",true = "Newcastle",false = opponent_round_k_16$opponent) 
  
  #Team
  for (i in seq(from = 2,by = 1, to = upper_lim)) {
    
    team_round_temp <- data.frame(index = 1:625)
    team_round_temp[,2] <- team_round_16[,(i+k-1)]
    
    colnames(team_round_temp)[2] <- "team"
    
    if(i == 2){
      team_round_k_16 <- team_round_temp
    } else{
      team_round_k_16    <- rbind(team_round_k_16,team_round_temp)  
    }
    
  }
  
  team_round_k_16 <- team_round_k_16 %>% select(team)
  
  #Cost
  for (i in seq(from = 2,by = 1, to = upper_lim)) {
    
    cost_round_temp <- data.frame(index = 1:625)
    cost_round_temp[,2] <- cost_round_16[,(i+k-1)]
    
    colnames(cost_round_temp)[2] <- "cost"
    
    if(i == 2){
      cost_round_k_16 <- cost_round_temp
    } else{
      cost_round_k_16    <- rbind(cost_round_k_16,cost_round_temp)  
    }
    
  }
  
  cost_round_k_16 <- cost_round_k_16 %>% select(cost)
  
  #Pos
  for (i in seq(from = 2,by = 1, to = upper_lim)) {
    
    pos_round_temp <- data.frame(index = 1:625)
    pos_round_temp[,2] <- pos_round_16[,((i+k-1))]
    
    colnames(pos_round_temp)[2] <- "pos"
    
    if(i == 2){
      pos_round_k_16 <- pos_round_temp
    } else{
      pos_round_k_16    <- rbind(pos_round_k_16,pos_round_temp)  
    }
    
  }
  
  pos_round_k_16 <- pos_round_k_16 %>% select(pos)
  
  #Transfers in
  for (i in seq(from = 2,by = 1, to = upper_lim)) {
    
    trans_in_round_temp <- data.frame(index = 1:625)
    trans_in_round_temp[,2:(k+1)] <- trans_in_round_16[,i:(i+(k-1))]
    
    colnames(trans_in_round_temp)[2:(k)] <- paste0("trans_in_prev_",(k-1):1)
    colnames(trans_in_round_temp)[(k+1)] <- "realized"
    
    if(i == 2){
      trans_in_round_k_16 <- trans_in_round_temp
    } else{
      trans_in_round_k_16    <- rbind(trans_in_round_k_16,trans_in_round_temp)  
    }
    
  }
  
  trans_in_round_k_16 <- trans_in_round_k_16 %>% select(trans_in_prev_3,trans_in_prev_2,trans_in_prev_1)
  
  #Transfers out
  for (i in seq(from = 2,by = 1, to = upper_lim)) {
    
    trans_out_round_temp <- data.frame(index = 1:625)
    trans_out_round_temp[,2:(k+1)] <- trans_out_round_16[,i:(i+(k-1))]
    
    colnames(trans_out_round_temp)[2:(k)] <- paste0("trans_out_prev_",(k-1):1)
    colnames(trans_out_round_temp)[(k+1)] <- "realized"
    
    if(i == 2){
      trans_out_round_k_16 <- trans_out_round_temp
    } else{
      trans_out_round_k_16    <- rbind(trans_out_round_k_16,trans_out_round_temp)  
    }
    
  }
  
  trans_out_round_k_16 <- trans_out_round_k_16 %>% select(trans_out_prev_3,trans_out_prev_2,trans_out_prev_1)
  ######################
  
# Create regressors
##################
#Create regressors
  regressors_16 <- cbind(points_round_k_16,opponent_round_k_16,team_round_k_16,cost_round_k_16,
                         pos_round_k_16,trans_in_round_k_16,trans_out_round_k_16)
  #regressors_16$index <- as.factor(regressors_16$index)

  #regressors_16 <- regressors_16 %>% na.omit()
  #regressors_16 <- regressors_16[,-1]
  
  
  #indexes <- regressors_16[,"index"]
  #rownames(regressors_16) <- indexes
  
  
  
  #################
  


###################
# Fit model
###################

regressors_16 <- regressors_16 %>% na.omit()
  
options(stringsAsFactors = F)

library(ISLR)
summary(regressors_16)

#Best subset
library(leaps)
regfit.full = regsubsets(realized~.,data = regressors_16) # does not work

#Forward stepwise selection
regfit.fwd = regsubsets(realized~.,data = regressors_16,method = 'forward') # does not work with index
summary(regfit.fwd)
plot(regfit.fwd)

#### Forward selection 

nmax = dim(regressors_16)[2]

train = seq(1,round(0.8*dim(regressors_16)[1]))
regfit.fwd = regsubsets(realized~.,data = regressors_16[train,],method = 'forward',nvmax = 15) 

val.errors <- rep(NA,dim(regressors_16)[2])
x.test = model.matrix(realized~.,data=regressors_16[-train,])
for (i in 1:dim(regressors_16)[2]){
  coefi = coef(regfit.fwd,id = i)
  pred = x.test[,names(coefi)]%*%coefi
  val.errors[i] = mean((regressors_16$realized[-train]-pred)^2)
}

plot(sqrt(val.errors),ylab = "Root MSE",ylim = c(2.3,2.8),pch = 19,type = "b")
points(sqrt(regfit.fwd$rss[-1]/180))



##Model seletion based on Cross-validation (10 fold)
set.seed(11)
folds = sample(rep(1:10,length = nrow(regressors_16)))
folds
table(folds)

cv.erros = matrix(NA,10,nmax)
for (k in 1:10) {
  best.fit = regsubsets(realized~.,data = regressors_16[folds!=k,],nvmax = nmax,method = 'forward')
    for (i in 1:nmax) {
      pred = predict(best.fit,regressors_16[folds == k,],id = i)
      cv.erros[k,i] = mean((regressors_16$realized[folds==k]-pred)^2)
    }
}

rmse.cv = sqrt(apply(cv.erros,2,mean))
plot(rmse.cv,pch = 19,type = "b")



## Ridge and lasso regression
library(glmnet)
x = model.matrix(realized~.,-1,data = regressors_16)
y = regressors_16$realized

# Ridge:
fit.ridge = glmnet(x,y,alpha = 0)
plot(fit.ridge,xvar = 'lambda',label = T)
cv.ridge = cv.glmnet(x,y, alpha= 0)
plot(cv.ridge)

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



###########
# "Dummy" model:
###########


model_k = lm(realized ~ prev_4 + prev_3 + prev_2 + prev_1 + cost,data = regressors_16)
summary(model_k)















