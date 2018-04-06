##############
# Create variance covariance matrix
##############

points <- t(points_round_16)
res <- cov(points)
res_corr <- cor(points)


library(corrplot)
corrplot(res_corr, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

res_small <- as.data.frame(res_corr)
#er