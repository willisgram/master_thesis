##############
# Create variance covariance matrix
##############

library(tidyverse)
library(Hmisc)

# Forhold: 
# GK GK, GK DEF, GK MID, GK FWD
# DEF DEF, DEF MID, DEF FWD
# MID MID, MID FWD

# GK_DEF
####################
data <- inner_join(players,points_round_16,"index") %>% filter(PositionsList == "GLK" | PositionsList == "DEF")

data_sum <- data %>% group_by(PositionsList, Team) %>% summarise(mean_4 = mean(round_4,na.rm = T),
                                                                 mean_5 = mean(round_5,na.rm = T),
                                                                 mean_6 = mean(round_6,na.rm = T),
                                                                 mean_7 = mean(round_7,na.rm = T),
                                                                 mean_8 = mean(round_8,na.rm = T),
                                                                 mean_9 = mean(round_9,na.rm = T),
                                                                 mean_10 = mean(round_10,na.rm = T),
                                                                 mean_11 = mean(round_11,na.rm = T),
                                                                 mean_12 = mean(round_12,na.rm = T),
                                                                 mean_13 = mean(round_13,na.rm = T),
                                                                 mean_14 = mean(round_14,na.rm = T),
                                                                 mean_15 = mean(round_15,na.rm = T),
                                                                 mean_16 = mean(round_16,na.rm = T),
                                                                 mean_17 = mean(round_17,na.rm = T),
                                                                 mean_18 = mean(round_18,na.rm = T),
                                                                 mean_19 = mean(round_19,na.rm = T),
                                                                 mean_20 = mean(round_20,na.rm = T),
                                                                 mean_21 = mean(round_21,na.rm = T),
                                                                 mean_22 = mean(round_22,na.rm = T),
                                                                 mean_23 = mean(round_23,na.rm = T),
                                                                 mean_24 = mean(round_24,na.rm = T),
                                                                 mean_25 = mean(round_25,na.rm = T),
                                                                 mean_26 = mean(round_26,na.rm = T),
                                                                 mean_27 = mean(round_27,na.rm = T),
                                                                 mean_28 = mean(round_28,na.rm = T),
                                                                 mean_29 = mean(round_29,na.rm = T),
                                                                 mean_30 = mean(round_30,na.rm = T),
                                                                 mean_31 = mean(round_31,na.rm = T),
                                                                 mean_32 = mean(round_32,na.rm = T),
                                                                 mean_33 = mean(round_33,na.rm = T),
                                                                 mean_34 = mean(round_34,na.rm = T),
                                                                 mean_35 = mean(round_35,na.rm = T),
                                                                 mean_36 = mean(round_36,na.rm = T),
                                                                 mean_37 = mean(round_37,na.rm = T),
                                                                 mean_38 = mean(round_38,na.rm = T)) %>% mutate(
                                                                   name = paste(PositionsList,Team,sep = "_")
                                                                 )

data_sum  <- data_sum[,names(data_sum) != c("PositionsList","Team")]
data_sum_corr <- data_sum[,-36]
rownames(data_sum_corr) <- data_sum$name

data_sum_corr <- t(data_sum_corr)
res_corr <- cor(data_sum_corr)
res_corr_d <- res_corr[,21:40]
correlations <- diag(res_corr_d)
gk_def = mean(correlations,na.rm = T)


r_corr <- rcorr(res_corr)
r_corr_d <- r_corr$P[,21:40]
sig_levels <- diag(r_corr_d)

###########################

# GK_MID
####################
data <- inner_join(players,points_round_16,"index") %>% filter(PositionsList == "GLK" | PositionsList == "MID")

data_sum <- data %>% group_by(PositionsList, Team) %>% summarise(mean_4 = mean(round_4,na.rm = T),
                                                                 mean_5 = mean(round_5,na.rm = T),
                                                                 mean_6 = mean(round_6,na.rm = T),
                                                                 mean_7 = mean(round_7,na.rm = T),
                                                                 mean_8 = mean(round_8,na.rm = T),
                                                                 mean_9 = mean(round_9,na.rm = T),
                                                                 mean_10 = mean(round_10,na.rm = T),
                                                                 mean_11 = mean(round_11,na.rm = T),
                                                                 mean_12 = mean(round_12,na.rm = T),
                                                                 mean_13 = mean(round_13,na.rm = T),
                                                                 mean_14 = mean(round_14,na.rm = T),
                                                                 mean_15 = mean(round_15,na.rm = T),
                                                                 mean_16 = mean(round_16,na.rm = T),
                                                                 mean_17 = mean(round_17,na.rm = T),
                                                                 mean_18 = mean(round_18,na.rm = T),
                                                                 mean_19 = mean(round_19,na.rm = T),
                                                                 mean_20 = mean(round_20,na.rm = T),
                                                                 mean_21 = mean(round_21,na.rm = T),
                                                                 mean_22 = mean(round_22,na.rm = T),
                                                                 mean_23 = mean(round_23,na.rm = T),
                                                                 mean_24 = mean(round_24,na.rm = T),
                                                                 mean_25 = mean(round_25,na.rm = T),
                                                                 mean_26 = mean(round_26,na.rm = T),
                                                                 mean_27 = mean(round_27,na.rm = T),
                                                                 mean_28 = mean(round_28,na.rm = T),
                                                                 mean_29 = mean(round_29,na.rm = T),
                                                                 mean_30 = mean(round_30,na.rm = T),
                                                                 mean_31 = mean(round_31,na.rm = T),
                                                                 mean_32 = mean(round_32,na.rm = T),
                                                                 mean_33 = mean(round_33,na.rm = T),
                                                                 mean_34 = mean(round_34,na.rm = T),
                                                                 mean_35 = mean(round_35,na.rm = T),
                                                                 mean_36 = mean(round_36,na.rm = T),
                                                                 mean_37 = mean(round_37,na.rm = T),
                                                                 mean_38 = mean(round_38,na.rm = T)) %>% mutate(
                                                                   name = paste(PositionsList,Team,sep = "_")
                                                                 )

data_sum  <- data_sum[,names(data_sum) != c("PositionsList","Team")]
data_sum_corr <- data_sum[,-36]
rownames(data_sum_corr) <- data_sum$name

data_sum_corr <- t(data_sum_corr)
res_corr <- cor(data_sum_corr)
res_corr_d <- res_corr[,21:40]
correlations <- diag(res_corr_d)
gk_MID = mean(correlations,na.rm = T)

r_corr <- rcorr(res_corr)
r_corr_d <- r_corr$P[,21:40]
sig_levels <- diag(r_corr_d)












###########################


# GK_FWD
####################
data <- inner_join(players,points_round_16,"index") %>% filter(PositionsList == "GLK" | PositionsList == "FWD")

data_sum <- data %>% group_by(PositionsList, Team) %>% summarise(mean_4 = mean(round_4,na.rm = T),
                                                                 mean_5 = mean(round_5,na.rm = T),
                                                                 mean_6 = mean(round_6,na.rm = T),
                                                                 mean_7 = mean(round_7,na.rm = T),
                                                                 mean_8 = mean(round_8,na.rm = T),
                                                                 mean_9 = mean(round_9,na.rm = T),
                                                                 mean_10 = mean(round_10,na.rm = T),
                                                                 mean_11 = mean(round_11,na.rm = T),
                                                                 mean_12 = mean(round_12,na.rm = T),
                                                                 mean_13 = mean(round_13,na.rm = T),
                                                                 mean_14 = mean(round_14,na.rm = T),
                                                                 mean_15 = mean(round_15,na.rm = T),
                                                                 mean_16 = mean(round_16,na.rm = T),
                                                                 mean_17 = mean(round_17,na.rm = T),
                                                                 mean_18 = mean(round_18,na.rm = T),
                                                                 mean_19 = mean(round_19,na.rm = T),
                                                                 mean_20 = mean(round_20,na.rm = T),
                                                                 mean_21 = mean(round_21,na.rm = T),
                                                                 mean_22 = mean(round_22,na.rm = T),
                                                                 mean_23 = mean(round_23,na.rm = T),
                                                                 mean_24 = mean(round_24,na.rm = T),
                                                                 mean_25 = mean(round_25,na.rm = T),
                                                                 mean_26 = mean(round_26,na.rm = T),
                                                                 mean_27 = mean(round_27,na.rm = T),
                                                                 mean_28 = mean(round_28,na.rm = T),
                                                                 mean_29 = mean(round_29,na.rm = T),
                                                                 mean_30 = mean(round_30,na.rm = T),
                                                                 mean_31 = mean(round_31,na.rm = T),
                                                                 mean_32 = mean(round_32,na.rm = T),
                                                                 mean_33 = mean(round_33,na.rm = T),
                                                                 mean_34 = mean(round_34,na.rm = T),
                                                                 mean_35 = mean(round_35,na.rm = T),
                                                                 mean_36 = mean(round_36,na.rm = T),
                                                                 mean_37 = mean(round_37,na.rm = T),
                                                                 mean_38 = mean(round_38,na.rm = T)) %>% mutate(
                                                                   name = paste(PositionsList,Team,sep = "_")
                                                                 )

data_sum  <- data_sum[,names(data_sum) != c("PositionsList","Team")]
data_sum_corr <- data_sum[,-36]
rownames(data_sum_corr) <- data_sum$name

data_sum_corr <- t(data_sum_corr)
res_corr <- cor(data_sum_corr)
res_corr_d <- res_corr[,21:40]
correlations <- diag(res_corr_d)
gk_FWD = mean(correlations,na.rm = T)


r_corr <- rcorr(res_corr)
r_corr_d <- r_corr$P[,21:40]
sig_levels <- diag(r_corr_d)

###########################

# DEF_MID
####################
data <- inner_join(players,points_round_16,"index") %>% filter(PositionsList == "DEF" | PositionsList == "MID")

data_sum <- data %>% group_by(PositionsList, Team) %>% summarise(mean_4 = mean(round_4,na.rm = T),
                                                                 mean_5 = mean(round_5,na.rm = T),
                                                                 mean_6 = mean(round_6,na.rm = T),
                                                                 mean_7 = mean(round_7,na.rm = T),
                                                                 mean_8 = mean(round_8,na.rm = T),
                                                                 mean_9 = mean(round_9,na.rm = T),
                                                                 mean_10 = mean(round_10,na.rm = T),
                                                                 mean_11 = mean(round_11,na.rm = T),
                                                                 mean_12 = mean(round_12,na.rm = T),
                                                                 mean_13 = mean(round_13,na.rm = T),
                                                                 mean_14 = mean(round_14,na.rm = T),
                                                                 mean_15 = mean(round_15,na.rm = T),
                                                                 mean_16 = mean(round_16,na.rm = T),
                                                                 mean_17 = mean(round_17,na.rm = T),
                                                                 mean_18 = mean(round_18,na.rm = T),
                                                                 mean_19 = mean(round_19,na.rm = T),
                                                                 mean_20 = mean(round_20,na.rm = T),
                                                                 mean_21 = mean(round_21,na.rm = T),
                                                                 mean_22 = mean(round_22,na.rm = T),
                                                                 mean_23 = mean(round_23,na.rm = T),
                                                                 mean_24 = mean(round_24,na.rm = T),
                                                                 mean_25 = mean(round_25,na.rm = T),
                                                                 mean_26 = mean(round_26,na.rm = T),
                                                                 mean_27 = mean(round_27,na.rm = T),
                                                                 mean_28 = mean(round_28,na.rm = T),
                                                                 mean_29 = mean(round_29,na.rm = T),
                                                                 mean_30 = mean(round_30,na.rm = T),
                                                                 mean_31 = mean(round_31,na.rm = T),
                                                                 mean_32 = mean(round_32,na.rm = T),
                                                                 mean_33 = mean(round_33,na.rm = T),
                                                                 mean_34 = mean(round_34,na.rm = T),
                                                                 mean_35 = mean(round_35,na.rm = T),
                                                                 mean_36 = mean(round_36,na.rm = T),
                                                                 mean_37 = mean(round_37,na.rm = T),
                                                                 mean_38 = mean(round_38,na.rm = T)) %>% mutate(
                                                                   name = paste(PositionsList,Team,sep = "_")
                                                                 )

data_sum  <- data_sum[,names(data_sum) != c("PositionsList","Team")]
data_sum_corr <- data_sum[,-36]
rownames(data_sum_corr) <- data_sum$name

data_sum_corr <- t(data_sum_corr)
res_corr <- cor(data_sum_corr)
res_corr_d <- res_corr[,21:40]
correlations <- diag(res_corr_d)
def_mid = mean(correlations,na.rm = T)


r_corr <- rcorr(res_corr)
r_corr_d <- r_corr$P[,21:40]
sig_levels <- diag(r_corr_d)

###########################

# DEF_FWD
####################
data <- inner_join(players,points_round_16,"index") %>% filter(PositionsList == "DEF" | PositionsList == "FWD")

data_sum <- data %>% group_by(PositionsList, Team) %>% summarise(mean_4 = mean(round_4,na.rm = T),
                                                                 mean_5 = mean(round_5,na.rm = T),
                                                                 mean_6 = mean(round_6,na.rm = T),
                                                                 mean_7 = mean(round_7,na.rm = T),
                                                                 mean_8 = mean(round_8,na.rm = T),
                                                                 mean_9 = mean(round_9,na.rm = T),
                                                                 mean_10 = mean(round_10,na.rm = T),
                                                                 mean_11 = mean(round_11,na.rm = T),
                                                                 mean_12 = mean(round_12,na.rm = T),
                                                                 mean_13 = mean(round_13,na.rm = T),
                                                                 mean_14 = mean(round_14,na.rm = T),
                                                                 mean_15 = mean(round_15,na.rm = T),
                                                                 mean_16 = mean(round_16,na.rm = T),
                                                                 mean_17 = mean(round_17,na.rm = T),
                                                                 mean_18 = mean(round_18,na.rm = T),
                                                                 mean_19 = mean(round_19,na.rm = T),
                                                                 mean_20 = mean(round_20,na.rm = T),
                                                                 mean_21 = mean(round_21,na.rm = T),
                                                                 mean_22 = mean(round_22,na.rm = T),
                                                                 mean_23 = mean(round_23,na.rm = T),
                                                                 mean_24 = mean(round_24,na.rm = T),
                                                                 mean_25 = mean(round_25,na.rm = T),
                                                                 mean_26 = mean(round_26,na.rm = T),
                                                                 mean_27 = mean(round_27,na.rm = T),
                                                                 mean_28 = mean(round_28,na.rm = T),
                                                                 mean_29 = mean(round_29,na.rm = T),
                                                                 mean_30 = mean(round_30,na.rm = T),
                                                                 mean_31 = mean(round_31,na.rm = T),
                                                                 mean_32 = mean(round_32,na.rm = T),
                                                                 mean_33 = mean(round_33,na.rm = T),
                                                                 mean_34 = mean(round_34,na.rm = T),
                                                                 mean_35 = mean(round_35,na.rm = T),
                                                                 mean_36 = mean(round_36,na.rm = T),
                                                                 mean_37 = mean(round_37,na.rm = T),
                                                                 mean_38 = mean(round_38,na.rm = T)) %>% mutate(
                                                                   name = paste(PositionsList,Team,sep = "_")
                                                                 )

data_sum  <- data_sum[,names(data_sum) != c("PositionsList","Team")]
data_sum_corr <- data_sum[,-36]
rownames(data_sum_corr) <- data_sum$name

data_sum_corr <- t(data_sum_corr)
res_corr <- cor(data_sum_corr)
res_corr_d <- res_corr[,21:40]
correlations <- diag(res_corr_d)
def_fwd = mean(correlations,na.rm = T)


r_corr <- rcorr(res_corr)
r_corr_d <- r_corr$P[,21:40]
sig_levels <- diag(r_corr_d)

###########################

# MID_FWD
####################
data <- inner_join(players,points_round_16,"index") %>% filter(PositionsList == "MID" | PositionsList == "FWD")

data_sum <- data %>% group_by(PositionsList, Team) %>% summarise(mean_4 = mean(round_4,na.rm = T),
                                                                 mean_5 = mean(round_5,na.rm = T),
                                                                 mean_6 = mean(round_6,na.rm = T),
                                                                 mean_7 = mean(round_7,na.rm = T),
                                                                 mean_8 = mean(round_8,na.rm = T),
                                                                 mean_9 = mean(round_9,na.rm = T),
                                                                 mean_10 = mean(round_10,na.rm = T),
                                                                 mean_11 = mean(round_11,na.rm = T),
                                                                 mean_12 = mean(round_12,na.rm = T),
                                                                 mean_13 = mean(round_13,na.rm = T),
                                                                 mean_14 = mean(round_14,na.rm = T),
                                                                 mean_15 = mean(round_15,na.rm = T),
                                                                 mean_16 = mean(round_16,na.rm = T),
                                                                 mean_17 = mean(round_17,na.rm = T),
                                                                 mean_18 = mean(round_18,na.rm = T),
                                                                 mean_19 = mean(round_19,na.rm = T),
                                                                 mean_20 = mean(round_20,na.rm = T),
                                                                 mean_21 = mean(round_21,na.rm = T),
                                                                 mean_22 = mean(round_22,na.rm = T),
                                                                 mean_23 = mean(round_23,na.rm = T),
                                                                 mean_24 = mean(round_24,na.rm = T),
                                                                 mean_25 = mean(round_25,na.rm = T),
                                                                 mean_26 = mean(round_26,na.rm = T),
                                                                 mean_27 = mean(round_27,na.rm = T),
                                                                 mean_28 = mean(round_28,na.rm = T),
                                                                 mean_29 = mean(round_29,na.rm = T),
                                                                 mean_30 = mean(round_30,na.rm = T),
                                                                 mean_31 = mean(round_31,na.rm = T),
                                                                 mean_32 = mean(round_32,na.rm = T),
                                                                 mean_33 = mean(round_33,na.rm = T),
                                                                 mean_34 = mean(round_34,na.rm = T),
                                                                 mean_35 = mean(round_35,na.rm = T),
                                                                 mean_36 = mean(round_36,na.rm = T),
                                                                 mean_37 = mean(round_37,na.rm = T),
                                                                 mean_38 = mean(round_38,na.rm = T)) %>% mutate(
                                                                   name = paste(PositionsList,Team,sep = "_")
                                                                 )

data_sum  <- data_sum[,names(data_sum) != c("PositionsList","Team")]
data_sum_corr <- data_sum[,-36]
rownames(data_sum_corr) <- data_sum$name

data_sum_corr <- t(data_sum_corr)
res_corr <- cor(data_sum_corr)
res_corr_d <- res_corr[,21:40]
correlations <- diag(res_corr_d)
mid_fwd = mean(correlations,na.rm = T)


r_corr <- rcorr(res_corr)
r_corr_d <- r_corr$P[,21:40]
sig_levels <- diag(r_corr_d)

###########################















