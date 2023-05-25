
# FINISHED
rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\f")


allData <- c(3040, 29.2, 3840, 30.7, 2470, 24.7, 3800, 32.7,
3610, 32.3, 4600, 32.6, 3480, 31.3, 1900, 22.1, 3810, 31.5, 2530, 25.3,
2330, 24.5, 2920, 30.8, 1800, 19.9, 4990, 38.9, 3110, 27.3, 1670, 22.1,
3160, 27.1, 3310, 29.2, 2310, 24.0, 3450, 30.1, 4360, 33.8, 3600, 31.4,
1880, 21.5, 2850, 26.7, 3670, 32.2, 1590, 22.1, 1740, 22.5, 3770, 30.3,
2250, 27.5, 3850, 32.0, 2650, 25.6, 2480, 23.2, 4970, 34.5, 3570, 30.3,
2620, 26.2, 2620, 29.9, 2900, 26.7, 1890, 20.8, 1670, 21.1, 3030, 33.2,
2540, 24.1, 3030, 28.2)

n <- length(allData) / 2

par(mfrow=c(1,3)) # layout for a 1x3 plot layout

# Cleverly creating a vector for x and y values
densityX <- allData[c(allData < 1000)]
strengthY <- allData[c(allData > 1000)]

regressionModel <- lm(formula = strengthY ~ densityX)

plot(densityX, strengthY,
     main = "Strength vs Density", 
     xlab = "Strength", ylab = "Density")

# PART a)
abline(regressionModel, col = "purple", lwd = 2)
print(summary(regressionModel))

intercept <- unname(regressionModel[[1]][[1]])
slope <- unname(regressionModel[[1]][[2]])

x <- as.double(c(1 : 100))

# since the max value of density is less than n, I set it to an
# arbitrarily high number

yModel <- slope*x + intercept

cat("\n\na) Regression model: yHat = ", slope, "*x +", intercept, "\n")


# PART b)

SSE <- 0
for (i in 1 : n) {
  SSE = SSE + (strengthY[[i]] - yModel[[ densityX[[i]] ]])^2
}

estVar <- SSE / (n-2)
cat("b) Estimator of variance sigma^2 =", estVar, "\n")


# PART c)
sumYX <- 0

for (i in 1 : n) {
  sumYX = sumYX + densityX[i] * strengthY[i]
}

sumX <- sum(densityX)
sumY <- sum(strengthY)

sxy <- sumYX - ((sumY * sumX) / n)


Rsq <- summary(regressionModel)$r.squared # IMPORTANT TRICK

SST <- SSE + slope*sxy # OTHERWISE THIS WORKS TOO!!!

# Rsq <- 1 - (SSE/SST)
cat("c) Coefficient of Determination R^2 = ", Rsq, "\n")

# PART d)

ei <- c(strengthY - yModel[1:n])
plot(yModel[1:n], regressionModel[2][[1]][1:n],
     col = "red", xlab = "y_Estimated", ylab = "Residuals")
# plotting against yhat



plot(x[1:n], regressionModel[2][[1]][1:n],
     col = "skyblue", xlab = "x_new", ylab = "Residuals")
# plotting against the ind var x
writeLines("e) The red and blue dots on the plot are the residuals of yhat vs x respectively")


