
# FINISHED
rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\f")

xVals <- c(54, 54, 61, 61, 68, 68, 75, 75, 75)

yVals <- c(16.473, 18.693, 14.305, 15.121,
           13.505, 11.640, 11.168, 12.534, 11.224)

n <- length(xVals)

# PART a) Scatter Plot
plot(x = xVals, y = yVals,
     main = 'Deflection vs Stress Level',
     xlab = "Stress Level in % (x)", 
     ylab = "Deflection in mm (y)") # creating the scatter plot

# TESTING
linRegModel <- lm(formula = yVals ~ xVals,)

abline(linRegModel, col = "purple", lwd = 2)

# PART a) - least squares estimate of slope, intercept, and estimate of sigma^2
intercept <- unname(linRegModel[[1]][[1]]) # accesses the intercept
slope <- unname(linRegModel[[1]][[2]]) # accesses the slope

print(summary(linRegModel))

x <- c(1 : max(xVals)) # range of x extends to the max value of xVals

yModel <- slope*x + intercept

SSE <- 0
for (i in 1 : n) {
  SSE = SSE + (yVals[[i]] - yModel[[ xVals[[i]] ]])^2
}

estVar <- SSE / (n-2) # estimator of variance

cat("a) slope =", slope, "intercept =", intercept, "sigma^2 =", estVar, "\n")


# PART b)
stressLevel65 <-  yModel[[65]]
cat("b) Estimate of mean deflection if stress level is 65% = ",
    stressLevel65, "\n")


# PART c)
changeMean <- yModel[[6]] - yModel[[1]]
cat("c) Change in mean deflection with a 5% increment in stress level = ",
    changeMean, "\n")

# PART d)
stressIncr <- ((1 - intercept) / slope) - 
  ((2 - intercept) / slope)
cat("d) Increase in stress to decrease mean deflection by 1 mm =",
    stressIncr, "\n")

# PART e)
stressLevel68 <- yModel[[68]]
residual68 <- yVals[[5]] - yModel[[68]]
  
cat("e) Deflection if the stress level is 68% = ", stressLevel68, "\n
    The corresponding residual: ei = yi - yhati =", residual68)


