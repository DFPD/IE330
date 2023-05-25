# FINISHED
rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\f")

m1 <- c(2.7, 4.6, 2.6, 3.0, 3.2, 3.8)
m2 <- c(4.9, 4.6, 5.0, 4.2, 3.6, 4.2)
m3 <- c(4.6, 3.4, 2.9, 3.5, 4.1, 5.1)


Combined_Groups <- data.frame(cbind(m1, m2, m3))

Stacked_Groups <- stack(Combined_Groups)

AnovaStuff <- aov(values ~ ind, data = Stacked_Groups)

print(summary(AnovaStuff))


