library(xtable)
library(stargazer)
library(ggthemes)


print(xtable(total_points_round,digits = 0,align = rep("r",4)), include.rownames=FALSE)


res <- xtable(x = total_points_round, include.rownames=FALSE)

g <- ggplot(data = total_points_round,aes(x = round)
            ) + geom_line(aes(y = points, colour = "Net")
                  ) + geom_line(aes(y = points_brutto,colour = "Gross")
                        ) + labs(x = "Round", y = "Points"
                      ) + scale_x_continuous(
                        limits = c(15,25),
                        breaks = 15:25) + scale_color_manual(
                          name = "", 
                          values = c("red","blue")
                          ) + theme_base() + ggtitle("Points Round 15-25 2016/2017")
name_plot <- paste0("points_15_to_25.png")
path_plot <- '../../../../latex_master_thesis/fig/'
file_plot <- paste0(path_plot, name_plot)

ggsave(file_plot)



