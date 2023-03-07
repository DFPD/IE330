
# FINISHED
cat("\f")

b1Data <- c(724, 718, 776, 760, 745, 759, 795, 756, 742, 740, 761,
            749, 739, 747, 742)
n1 <- length(b1Data)

b2Data <- c(735, 775, 729, 755, 783, 760, 738, 780)
n2 <- length(b2Data)


stdb1 <- 19.128
stdb2 <- 21.2833

xBarb1 <- mean(b1Data)
xBarb2 <- mean(b2Data)

CI <- 90
alpha <- (100 - CI) / 100
alphaHalf <-  alpha / 2

zscore <- ((xBarb1 - xBarb2) - 10 )/ (sqrt(stdb1^2/n1 + stdb2^2/n2))

zstar <- qnorm(alpha, lower.tail = TRUE)
zstarHalf <- qnorm(alphaHalf, lower.tail = FALSE)

pVal <- pnorm(q = zscore, lower.tail = TRUE)

CILowerBound <- xBarb1 - xBarb2 - zstarHalf * 
  sqrt(stdb1^2/n1 + stdb2^2/n2)

CIUpperBound <- xBarb1 - xBarb2 + zstarHalf * 
  sqrt(stdb1^2/n1 + stdb2^2/n2)

writeLines("Here is your 2-sided confidence integral:")
cat(CILowerBound, " <= u1 - u2 <= ", CIUpperBound, "")



