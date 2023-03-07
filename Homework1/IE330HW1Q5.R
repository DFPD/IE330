
#FINISHED
cat("\f")

std <- 0.37
n <-  51
alpha <-  0.05
alphaHalf <- alpha / 2
alphaOpp <- 1 - alphaHalf

chiSqLower <- qchisq(alphaHalf, n-1, lower.tail = FALSE)
chiSqUpper <- qchisq(alphaOpp, n-1, lower.tail = FALSE)


lowerBound <-  sqrt(((n - 1) * std^2) / (chiSqLower))
upperBound <- sqrt(((n - 1) * std^2) / (chiSqUpper))

cat(lowerBound, " <= u <= ", upperBound)
