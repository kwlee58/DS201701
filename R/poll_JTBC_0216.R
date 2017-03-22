candidates <- c("문재인", "안희정", "이재명", "안철수", "황교안")
week2 <- c(57, 20, 11, 2, 1)
week3 <- c(61, 24, 7, 1, 1)
rates.df <- data.frame(candidates = candidates, week2 = week2, week3 = week3)
rates.df
# par(family = "HCR Dotum LVT")
b1 <- barplot(t(as.matrix(rates.df[, 2:3])), 
        axes = FALSE, 
        ylim = c(0, 65), 
        beside = TRUE, 
        names.arg = rates.df[, 1], 
        legend.text = c("2주차", "3주차"), col = c("grey", "blue"))
# axis(side = 2, 
#     at = as.vector(as.matrix(rates.df[, 2:3])), 
#     labels =  as.vector(as.matrix(rates.df[, 2:3])), las = 1)
text(x = b1[1, ], y = week2 + 2, labels = week2)
text(x = b1[2, ], y = week3 + 2, labels = week3, col = "blue")
main.title <- "대선후보 지지도(%)"
sub.title <- "JTBC 정치부회의, 한국갤럽 2017. 7-9일, 14-16일"
main.text <- "지지정당 : 더불어민주당"
title(main = main.title, sub = sub.title, cex.main = 2)
text(8, 50, main.text, cex = 1.5)
library(reshape2)
rates.df$candidates.f <- factor(candidates, levels = candidates)
rates.df
str(rates.df)
rates.df.melt <- melt(rates.df, 
                      id.vars = "candidates.f", 
                      measure.vars = c("week2", "week3"), 
                      variable.name = "week", value.name = "rates")
library(ggplot2)
g0 <- ggplot(data = rates.df.melt, 
             mapping = aes(x = candidates.f, y = rates, fill = week)) 
g1 <- g0 + 
  geom_bar(stat = "identity", position = "dodge") 
g1
g2 <- g1 +
  geom_text(mapping = aes(x = candidates.f, 
                          y = rates + 2, 
                          label = rates), 
            position = position_dodge(width = 1), 
            size = 5)
g2
g3 <- g2 +
  theme_bw(base_family = "")
g3
g4 <- g3 + 
    scale_fill_manual(name = "", 
                      values = c("grey", "blue"), 
                      labels = c("2월 2주차", "2월 3주차"))
g4
g5 <- g4 + 
    scale_x_discrete(name = "대선후보")
g5
g6 <- g5 +
    scale_y_continuous(name = "지지도", 
                       breaks = as.vector(as.matrix(rates.df[, 2:3])), 
                       labels = as.vector(as.matrix(rates.df[, 2:3])))
g6
g7 <- g6 +
    labs(title = main.title, subtitle = sub.title)
g7
g8 <- g7 +
  theme(plot.title = element_text(hjust = 0.5))
g8

