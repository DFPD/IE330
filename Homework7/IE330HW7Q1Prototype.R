# FINISHED
rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\f")

a <- 2
b <- 3
n <- 3

paint <- c(1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 2, 2)
dryingTime <- c(20, 20, 20, 20, 20, 20,
                25, 25, 25, 25, 25, 25, 30, 30, 30, 30, 30, 30)
finish <- c(74, 64, 50, 92, 86, 68, 73, 61, 44, 98, 73, 88, 78, 85, 92, 66, 45, 85)

Combined_Groups <- data.frame(cbind(paint, dryingTime, finish))


# Creating a variable as a factor for the ANOVA

Combined_Groups$paint <- as.factor(Combined_Groups$paint)
Combined_Groups$dryingTime <- as.factor(Combined_Groups$dryingTime)

anova <- aov(finish ~ paint * dryingTime, data = Combined_Groups)
print(summary(anova))