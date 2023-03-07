
#FINISHED

cat("\f")
values <- c(2216, 2237, 2249, 2204,
            2225, 2301, 2281, 2263,
            2318, 2255, 2275, 2295)
n <- length(values)

std <- sd(values) * sqrt((n - 1) / n)
xBar <- mean(values)

CI <- 95
alpha <- (100 - CI) / 100
alphaHalf <-  alpha / 2

tVal <- qt(alphaHalf, n-1, lower.tail = FALSE)

# Lower and upper bounds for part a)
twoSidedLB <- xBar - (tVal * std / sqrt(n))
twoSidedUB <- xBar + (tVal * std / sqrt(n))

tValPartB <- qt(alpha, n-1, lower.tail = FALSE)

oneSidedLB <- xBar - (tValPartB * std / sqrt(n))

writeLines("Here is your 2-sided confidence integral:")
cat(twoSidedLB, " <= u <= ", twoSidedUB, "\n")

writeLines("Here is your one-sided lower confidence bound:")
cat(oneSidedLB, " <= u")

