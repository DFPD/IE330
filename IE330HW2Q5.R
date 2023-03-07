
#FINISHED
cat("\f")

samples <- c(23.01, 22.22, 22.04, 22.62, 22.59)

n <- length(samples)
xBar <- mean(samples)
std <- sd(samples)

alpha <- 0.05
alphaHalf <- alpha / 2

tKnot <- (xBar - 22.5) / (std / sqrt(n))

pVal <- 2 * pt(tKnot, n-1, lower.tail = TRUE)

tCritTop <- qt(alphaHalf, n-1, lower.tail = FALSE)
tCritBottom <- qt(alphaHalf, n-1, lower.tail = TRUE)

CILowerBound <- xBar - tCritTop*(std/sqrt(n))
CIUpperBound <- xBar + tCritTop*(std/sqrt(n))

cat("Your confidence interval is: ", CILowerBound, "<= u <=", CIUpperBound)