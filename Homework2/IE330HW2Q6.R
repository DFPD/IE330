
#FINISHED
cat("\f")

samples <- c(18.0, 30.7, 19.8, 27.1, 22.3, 18.8, 31.8, 23.4, 21.2,
             27.9, 31.9, 27.1, 25.0, 24.7, 26.9, 21.8, 29.2, 34.8,
             26.7, 31.6)

n <- length(samples)
xBar <- mean(samples)
std <- sd(samples)

alpha <- 0.01

tKnot <- (xBar - 25) / (std / sqrt(n))

tCrit <- qt(alpha, n-1, lower.tail = FALSE)

pVal <- pt(tKnot, n-1, lower.tail = FALSE)

CILowerBound <- xBar - tCrit * (std / sqrt(n))

cat("Your lower bounded CI is", CILowerBound, "<= u")