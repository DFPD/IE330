# FINISHED
rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\f")

y <- c(240, 236, 270, 274, 301, 316, 300, 296, 267, 276, 288, 261)
x1 <- c(25, 31, 45, 60, 65, 72, 80, 84, 75, 60, 50, 38)
x2 <- c(24, 21, 24, 25, 25, 26, 25, 25, 24, 25, 25, 23)
x3 <- c(91, 90, 88, 87, 91, 94, 87, 86, 88, 91, 90, 89)
x4 <- c(100, 95, 110, 88, 94, 99, 97, 96, 110, 105, 100, 98)
n <- length(y)

equation1 <- c(n, sum(x1), sum(x2), sum(x3), sum(x4))
equation2 <- c(sum(x1), sum(x1^2), sum(x1*x2), sum(x1*x3), sum(x1*x4))
equation3 <- c(sum(x2), sum(x2*x1), sum(x2^2), sum(x2*x3), sum(x2*x4))
equation4 <- c(sum(x3), sum(x3*x1), sum(x3*x2), sum(x3^2), sum(x3*x4))
equation5 <- c(sum(x4), sum(x4*x1), sum(x4*x2), sum(x4*x3), sum(x4^2))

equals <- c(sum(y), sum(x1*y), sum(x2*y), sum(x3*y), sum(x4*y))

equations <- rbind(equation1, equation2, equation3, equation4, equation5)

regVars <- solve(equations, equals) # regression variables

# regModel <- lm(y ~ x1 + x2 + x3 + x4)
# This stuff worked but I decided to manually code it
# print(summary(regModel))


# Part a)
b0 <- regVars[[1]]
b1 <- regVars[[2]]
b2 <- regVars[[3]]
b3 <- regVars[[4]]
b4 <- regVars[[5]]

SSE <- 0
for (i in 1 : n) {
  SSE = SSE +
    (y[[i]] - (b0 + b1*x1[[i]] + b2*x2[[i]]
                   + b3*x3[[i]] + b4*x4[[i]]) )^2
}
cat("a) Multiple Linear Regression Model: y =", b0, "+", b1, 'x1 + ', b2, 'x2 + ',
    b3, "x3 + ", b4, "x4\n")


# Part b)
estVar <- SSE / (n - 5)
cat("b) Estimator of variance = ", estVar, "\n")


# Part c)

yEstimated <- regVars[[1]] + regVars[[2]]*75 + regVars[[3]]*24 +
  regVars[[4]]*90 + regVars[[5]]*98

cat("c) Predicted power consumption for x1 = 75 deg F, x2 = 24 Days, x3 = 90%,
    and x4 = 98 tons = ", yEstimated)



