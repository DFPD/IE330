
# FINISHED
cat("\f")

values <- c(2.69, 5.76, 2.67, 1.62, 4.12)

n <- length(values)
std <- 0.66

CI <- 95
alpha <- (100 - CI) / 100
alphaHalf <-  alpha / 2
xBar <- mean(values)
error <- 0.55 # value of E for part b)

zStar <- qnorm(alphaHalf, lower.tail = FALSE)

twoSLB <-  xBar - zStar * (std / sqrt(n)) # bounds for 2-sided CI
twoSUB <-  xBar + zStar * (std / sqrt(n))

n <- ceiling(((zStar * std) / error) ^ 2) # round up to nearest integer


writeLines("Here is your 2-sided confidence integral:")
cat(twoSLB, "dyn-cm^2 <= u <= ", twoSUB, "dyn-cm^2\n")
cat("You should take n =", n, "number of observations for the CI to be",
"no wider than 55 dyne-cm^2")


