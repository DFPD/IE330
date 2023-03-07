
# FINISHED
cat("\f")
n <- 15
std <- 0.001

CI <- 99
alpha <- (100 - CI) / 100
alphaHalf <-  alpha / 2
xBar <- 74.036

zStar <- qnorm(alphaHalf, lower.tail = FALSE)

twoSLB <-  xBar - zStar * (std / sqrt(n)) # bounds for 2-sided CI
twoSUB <-  xBar + zStar * (std / sqrt(n))

# new zStar for part b)
zStarB <-  qnorm(alpha, lower.tail = FALSE)
oneSLB <- xBar - zStarB * (std / sqrt(n)) # one-sided lower bound


writeLines("Here is your 2-sided confidence integral:")
cat(twoSLB, "mm. <= u <= ", twoSUB, "mm.")
writeLines("\nHere is your 1-sided lower-confidence bound:")
cat(oneSLB, "mm. <= u")


