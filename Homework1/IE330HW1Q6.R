
#FINISHED
cat("\f")

values <- c(101, 104, 104, 77, 89, 88, 104, 96, 82, 70, 89, 91, 39, 103, 93, 
               85, 104, 104, 81, 67, 104, 104, 104, 87, 104, 89, 78, 104, 86,
               76, 103, 102, 80, 45, 94, 104, 104, 76, 80, 72, 73)
  
n <- length(values)

std <- sd(values) * sqrt((n - 1) / n)

alpha <-  0.05
alphaHalf <- alpha / 2
alphaOpp <- 1 - alphaHalf

chiSqLower <- qchisq(alphaHalf, n-1, lower.tail = FALSE)
chiSqUpper <- qchisq(alphaOpp, n-1, lower.tail = FALSE)


lowerBound <-  sqrt(((n - 1) * std^2) / (chiSqLower))
upperBound <- sqrt(((n - 1) * std^2) / (chiSqUpper))

cat("Confidence Interval: ", lowerBound, " <= u <= ", upperBound)
