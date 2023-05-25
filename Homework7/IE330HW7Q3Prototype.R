# FINISHED
rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\f")

a <- 3
b <- 3
n <- 4

freq <- c(10, 10, 10, 10, 1, 1, 1, 1, 0.1, 0.1, 0.1, 0.1,
          10, 10, 10, 10, 1, 1, 1, 1, 0.1, 0.1, 0.1, 0.1,
          10, 10, 10, 10, 1, 1, 1, 1, 0.1, 0.1, 0.1, 0.1)
environ <- c('Air','Air','Air','Air','Air','Air','Air','Air','Air','Air','Air','Air',
             'H2O','H2O','H2O','H2O','H2O','H2O','H2O','H2O','H2O','H2O','H2O','H2O',
             'SaltH2O','SaltH2O','SaltH2O','SaltH2O','SaltH2O','SaltH2O','SaltH2O','SaltH2O','SaltH2O','SaltH2O','SaltH2O','SaltH2O')
crackGrowth <- c(2.29, 2.47, 2.48, 2.12, 2.65, 2.68, 2.06, 2.38, 2.24, 2.71, 2.81, 2.08,
                 2.06, 2.05, 2.23, 2.03, 3.20, 3.18, 3.96, 3.64, 11, 11, 9.06, 11.3,
                 1.9, 1.93, 1.75, 2.06, 3.1, 3.24, 3.98, 3.24, 9.96, 10.01, 9.36, 10.40)

Combined_Groups <- data.frame(cbind(freq, environ, crackGrowth))


# Creating a variable as a factor for the ANOVA

Combined_Groups$freq <- as.factor(Combined_Groups$freq)
Combined_Groups$environ <- as.factor(Combined_Groups$environ)

anova <- aov(crackGrowth ~ freq * environ, data = Combined_Groups)
print(summary(anova))


lnCrack <- c()
for(i in 1 : 18) {
  lnCrack <- c(lnCrack, log(crackGrowth[1]))
}
lnCombo <- data.frame(cbind(freq, environ, lnCrack))
lnanova <- aov(lnCrack ~ freq * environ, data = lnCombo)
cat('\n\nANOVA with ln(y) is as follows:\n')
print(summary(lnanova))