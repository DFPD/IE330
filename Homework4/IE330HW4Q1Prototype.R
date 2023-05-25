
# UNFINISHED
cat("\f")
rm(list=ls())
if(!is.null(dev.list())) dev.off()

xVals <- c(60, 63, 65, 70, 70, 70, 80, 90, 80, 80,
           85, 89, 90, 90, 90, 90, 94, 100, 100, 100)

yVals <- c(1, 0, 1, 2, 5, 1, 4, 6, 2, 3,
           5, 4, 6, 8, 4, 5, 7, 9, 7, 6)

n <- length(xVals)

par(mfrow=c(1,3)) # layout for a 1x2 plot layout


# PART a) Scatter Plot
plot(x = xVals, y = yVals) # creating the scatter plot

# TESTING
relation <- lm(formula = yVals ~ xVals,)

abline(relation, col = "purple", lwd = 2)

intercept <- unname(relation[[1]][[1]])
slope <- unname(relation[[1]][[2]])

x <- c(1:max(xVals))

yModel <- slope*x + intercept

print(summary(relation))

plot(x = c(1:n), relation[2][[1]][1:n], xlab = "x_new", ylab = "Residuals")

plot(yModel[1:n], relation[2][[1]][1:n], xlab = "y_estimated", ylab = "Residuals")
