# FINISHED
rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\f")

densitY <- c(0.749, 0.798, 0.849, 0.877, 0.929, 0.963, 0.997, 1.046,
             1.133, 1.17, 1.215)

x1 <- c(2.05, 2.15, 2.25, 2.3, 2.4, 2.47, 2.54, 2.64, 2.85, 2.94, 3.05)

x2 <- c(0.016, 0.02, 0.022, 0.023, 0.026, 0.028, 0.031, 0.034, 0.039,
        0.042, 0.045)
n <- length(densitY)

regModel <- lm(densitY ~ x1 + x2)

# Part a)
b0 <- unname(regModel[[1]][[1]])
b1 <- unname(regModel[[1]][[2]])
b2 <- unname(regModel[[1]][[3]])

regVars <- c(b0, b1, b2)
cat("a) Multiple Linear Regression Model: y =", b0, "+", b1, 'x1 + ', b2, 'x2\n')


# Part b)
SSE <- 0
for (i in 1 : n) {
  SSE = SSE +
    (densitY[[i]] - (regVars[[1]] + regVars[[2]]*x1[[i]] + regVars[[3]]*x2[[i]]) )^2
}

estVar <- SSE / (n - 3)
cat("b) Estimator of Variance:", estVar, "\n")


# Part c)

yEstimated <- regVars[[1]] + regVars[[2]]*2.5 + regVars[[3]]*0.03
cat("c) Density when x1 = 2.5 and x2 = 0.03:", yEstimated, "\n")


