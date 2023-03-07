
# FINISHED
cat("\f")

n1 <- 12
xBar1 <- 86
std1 <- 3

n2 <- 15
xBar2 <- 89
std2 <- 2

CI <- 99
alpha <- (100 - CI) / 100
alphaHalf <-  alpha / 2

pooledEstimator <- ((n1 - 1) * std1^2 + (n2 - 1) * std2^2) / 
  (n1 + n2 - 2)

pooledStd <- sqrt(pooledEstimator) # solving for "Sp"

tscore <- ((xBar1 - xBar2) - 0)/ (pooledStd * sqrt(1/n1 + 1/n2))

tCrit <- qt(alpha, n1 + n2 - 2, lower.tail = TRUE)

tCritHalf <- qt(alphaHalf, n1 + n2 - 2, lower.tail = FALSE)

CILowerBound <- xBar1 - xBar2 - tCritHalf *
  pooledStd * sqrt(1/n1 + 1/n2)

CIUpperBound <- xBar1 - xBar2 + tCritHalf *
  pooledStd * sqrt(1/n1 + 1/n2)


writeLines("Here is your 2-sided confidence integral:")
cat(CILowerBound, " <= u1 - u2 <= ", CIUpperBound, "")



