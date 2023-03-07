
#FINISHED
cat("\f")

n <- 17

std <- 0.09

chiSq <- (n - 1)*0.09^2 / 0.75^2

alpha <- 0.05

chiCrit <- qchisq(alpha, n - 1, lower.tail = FALSE)

CILowerBound <- (n - 1)*std^2/ chiCrit

boundedStd <- sqrt(CILowerBound)

cat("Your lower bounded confidence interval is sigma >= ", boundedStd)