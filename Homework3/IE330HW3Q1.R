
# FINISHED
cat("\f")
n <- 10


m1Data <- c(16.03, 16.01, 16.04, 15.96, 16.05, 15.98,
               16.05, 16.02, 16.02, 15.99)
m2Data <- c(16.02, 16.03, 15.97, 16.04, 15.96, 16.02,
               16.01, 16.01, 15.99, 16.00)


stdM1 <- 0.0302
stdM2 <- 0.0254

xBarM1 <- mean(m1Data)
xBarM2 <- mean(m2Data)

CI <- 95
alpha <- (100 - CI) / 100
alphaHalf <-  alpha / 2

zscore <- (xBarM1 - xBarM2) / (sqrt(stdM1^2/10 + stdM2^2/10))

zstar <- qnorm(alphaHalf, lower.tail = FALSE)

pVal <- 2 * pnorm(q = zscore, lower.tail = FALSE)

CILowerBound <- xBarM1 - xBarM2 - zstar * 
  sqrt(stdM1^2/10 + stdM2^2/10)

CIUpperBound <- xBarM1 - xBarM2 + zstar * 
  sqrt(stdM1^2/10 + stdM2^2/10)

writeLines("Here is your 2-sided confidence integral:")
cat(CILowerBound, "oz <= u1 - u2 <= ", CIUpperBound, "oz")



