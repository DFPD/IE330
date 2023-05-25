# FINISHED
rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\f")


y <- c(293, 230, 172, 91, 113, 125)
x1 <- c(1.6, 15.5, 22.0, 43.0, 33.0, 40.0)
x2 <- c(851, 816, 1058, 1201, 1357, 1115)

n <- length(y)

regModel <- lm(y ~ x1 + x2)

# Part a)
# Getting coefficient values
b0 <- unname(regModel[[1]][[1]])
b1 <- unname(regModel[[1]][[2]])
b2 <- unname(regModel[[1]][[3]])
cat("a) Multiple Linear Regression Model: y =", b0, "+", b1, 'x1 + ', b2, 'x2\n')

# Part b)

regVars <- c(b0, b1, b2)

SSE <- 0
for (i in 1 : n) {
  SSE = SSE +
    (y[[i]] - (regVars[[1]] + regVars[[2]]*x1[[i]] + regVars[[3]]*x2[[i]]) )^2
}

estVar <- SSE / (n - 3)
cat("b) Estimator of Variance:", estVar, "\n")

# Part c)
yEstimate <- b0 + b1*25 + b2*1000
cat("c) Wear when x1 = 25 and x2 = 1000:", yEstimate, "\n")

# Part d)
x3 <- c(x1 * x2)

newRegModel <- lm(y ~ x1 + x2 + x3)

# Getting coefficient values
newb0 <- unname(newRegModel[[1]][[1]])
newb1 <- unname(newRegModel[[1]][[2]])
newb2 <- unname(newRegModel[[1]][[3]])
newb3 <- unname(newRegModel[[1]][[4]])

newRegVars <- c(newb0, newb1, newb2, newb3)
cat("d) Multiple Linear Regression Model with interaction term: y =",
    newb0, "+", newb1, 'x1 + ', newb2, 'x2 + ', newb3, 'x1x2\n')


# Part e)

newSSE <- 0
for (i in 1 : n) {
  newSSE = newSSE +
    (y[[i]] - (newRegVars[[1]] + newRegVars[[2]]*x1[[i]] +
                 newRegVars[[3]]*x2[[i]] + newRegVars[[4]]*x3[[i]]) )^2
}

newEstVar <- newSSE / (n - 4)
cat("e) New estimator of variance: ", newEstVar, "\n")


# Part f)

newYEstimate <- newRegVars[[1]] + newRegVars[[2]]*25 + newRegVars[[3]]*1000 +
  newRegVars[[4]]*25*1000
cat("f) New value of wear when x1 = 25 and x2 = 1000:", newYEstimate, "\n")
