
# FINISHED
cat("\f")

data1 <- c(17, 16, 21, 14, 18, 24, 16, 14, 21, 23, 13, 18)
data2 <- c(18, 14, 19, 11, 23, 21, 10, 13, 19, 24, 15, 20)

n <- length(data1)

difference <- mapply('-', data1, data2, SIMPLIFY = FALSE)
dBar <- mean(unlist(difference)) # mean of the differences

stdDifference <- sd(unlist(difference)) # std dev for the difference in means

CI <- 95
alpha <- (100 - CI) / 100
alphaHalf <-  alpha / 2

tCrit <- qt(alphaHalf, n - 1, lower.tail = FALSE)

CILowerBound <- dBar - tCrit * (stdDifference / sqrt(n)) 
CIUpperBound <- dBar + tCrit * (stdDifference / sqrt(n))

writeLines("Here is your 2-sided confidence integral:")
cat(CILowerBound, " <= uD <= ", CIUpperBound, "")



