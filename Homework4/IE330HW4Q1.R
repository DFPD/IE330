
# FINISHED
rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\f")

# This question was coded manually for practice and error verification

xVals <- c(60, 63, 65, 70, 70, 70, 80, 90, 80, 80,
           85, 89, 90, 90, 90, 90, 94, 100, 100, 100)

yVals <- c(1, 0, 1, 2, 5, 1, 4, 6, 2, 3,
           5, 4, 6, 8, 4, 5, 7, 9, 7, 6)

n <- length(xVals)

par(mfrow=c(1,3)) # layout for a 1x3 plot layout

# PART a) Scatter Plot
plot(x = xVals, y = yVals,
     main = 'Blood Pressure Rise vs. Sound Pressure Level',
     xlab = "Sound Pressure Level in DB (x)", 
     ylab = "BP Rise in mmHg (y)") # creating the scatter plot

# PART b) least squares method
sumYX <- 0
for (i in 1 : n) {
  sumYX = sumYX + xVals[i] * yVals[i]
}

sumY <- sum(yVals)

sumX <- sum(xVals)

sumXSq <- 0

for (i in 1 : n) {
  sumXSq = sumXSq + xVals[i]^2
}

sxy <- sumYX - ((sumY * sumX) / n)
sxx <-  sumXSq - (sumX^2 / n)

slope <- sxy / sxx

yBar <- (1/n) * sumY
xBar <- (1/n) * sumX

intercept <- yBar - (slope * xBar)

x <- c(1 : max(xVals))

yModel <-  slope*x + intercept #

SSE <- 0
for (i in 1 : n) {
  SSE = SSE + (yVals[[i]] - yModel[[ xVals[[i]] ]])^2
}

estVar <- SSE / (n - 2) # estimator of variance
  

# plotting the line graph OVER the scatter plot
lines(x, yModel, col = "magenta", lwd = 2)
cat("a) and b) are shown on the graph with the linear regression model
overlaid on the scatter plot. A simple linear regression model does seem
appropriate\nFor part b) Sigma^2 =", estVar, "\n")

# PART c)

predMean = yModel[[85]] # DEBUG!!!
cat("c) Predicted mean with a DB of 85 = ", predMean, "\n")

# PART d)

SST <- SSE + slope*sxy

Rsq <- 1 - (SSE / SST) # Coefficient of Determination
cat("d) Coefficient of Determination R^2 = ", Rsq, "\n")

# PART e)

ei <- 0
for (i in 1 : n) {
  ei[[i]] <- yVals[[i]] - yModel[[ xVals[[i]] ]]
}

plot(yModel[1:n], ei, col = "blue")
plot(x[1:n], ei, col = "red")
writeLines("e) The red and blue dots on the plot are the residuals vs yhat and x, respectively")


