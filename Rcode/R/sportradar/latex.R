library(xtable)
library(stargazer)


print(xtable(total_points_round,digits = 0,align = rep("r",4)), include.rownames=FALSE)


res <- xtable(x = total_points_round, include.rownames=FALSE)

g <- ggplot(data = total_points_round) + 





