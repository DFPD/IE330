# FINISHED
rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\f")


m1 <- c(14.8, 14.8, 14.7, 14.8, 14.9)
m2 <- c(14.6, 15.0, 14.9, 14.8, 14.7)
m3 <- c(12.7, 11.6, 12.4, 12.7, 12.1)
m4 <- c(14.2, 14.4, 14.4, 12.2, 11.7)

Combined_Groups <- data.frame(cbind(m1, m2, m3, m4))

Stacked_Groups <- stack(Combined_Groups)

AnovaStuff <- aov(values ~ ind, data = Stacked_Groups)

print(summary(AnovaStuff))
