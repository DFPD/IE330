
#FINISHED
cat("\f")
n <- 16
std <- 3645.94

CI <- 95
alpha <- (100 - CI) / 100
alphaHalf <-  alpha / 2
xBar <- 60139.7

zStar <- qnorm(alphaHalf, lower.tail = FALSE)

twoSLB <-  xBar - zStar * (std / sqrt(n)) # bounds for 2-sided CI
twoSUB <-  xBar + zStar * (std / sqrt(n))

writeLines("Here is your 2-sided confidence integral:")
cat(twoSLB, "km. <= u <= ", twoSUB, "km.")

