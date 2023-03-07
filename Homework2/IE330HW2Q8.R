
#FINISHED
cat("\f")

n <- 51
std <- 0.37

var <- 0.0625

CI <- 95
alpha <- (100 - CI) / 100
alphaHalf <-  alpha / 2

chiSqKnot <- ((n - 1) * std^2) / 0.0625

chiCritHigh <- qchisq(alphaHalf, n - 1, lower.tail = FALSE)
chiCritLow <- qchisq(alphaHalf, n - 1, lower.tail = TRUE)

pVal <- 2* pchisq(chiSqKnot, n - 1, lower.tail = FALSE)

CILowerBound <- ((n - 1) * std^2 ) / chiCritHigh
CIUpperBound <- ((n - 1) * std^2 ) / chiCritLow

