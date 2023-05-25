# FINISHED
rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\f")


m1 <- c(8.2, 8, 8.2, 7.9, 8.1, 8)
m2 <- c(8.3, 8.4, 8.3, 8.2, 8.3, 8.1)
m3 <- c(8.9, 8.7, 8.9, 8.4, 8.3, 8.5)
m4 <- c(8.5, 8.7, 8.7, 8.7, 8.8, 8.8)
m5 <- c(8.8, 9.1, 9.0, 8.7, 8.9, 8.5)
m6 <- c(8.6, 8.5, 8.6, 8.7, 8.8, 8.8)


Combined_Groups <- data.frame(cbind(m1, m2, m3, m4, m5, m6))

Stacked_Groups <- stack(Combined_Groups)

AnovaStuff <- aov(values ~ ind, data = Stacked_Groups)

print(summary(AnovaStuff))

boxplot(Combined_Groups)

pValue <- pf(18.88, 5, 30, lower.tail = FALSE)

crossLinkerLevel <- c(-1, -0.75, -0.5, 0, 0.5, 1)
avgSpacing <- c(8.06667, 8.26667, 8.616667, 8.7, 8.8333, 8.86667)

plot(crossLinkerLevel, avgSpacing,
     main = "Avg. Domain Spacing vs. Cross-Linker Level",
     xlab = "Cross-Linker Level", ylab = "Mean Spacing")

