
# FINISHED
cat("\f")

n1 <-  10
std1 <- 4.7

n2 <- 16
std2 <- 5.8

CI <- 95
alpha <- (100 - CI) / 100
alphaHalf <-  alpha / 2

fKnot <- std1^2 / std2^2


fCritTop <- qf(alphaHalf, n1, n2, lower.tail = FALSE)
fCritBottom <- qf(alphaHalf, n1, n2, lower.tail = TRUE)

if (fKnot < fCritTop & fKnot > fCritBottom) {
  cat("We CANNOT reject the null hypothesis at alpha = ", alpha)
} else {
  cat("We CAN reject the null hypothesis at alpha = ", alpha)
}

