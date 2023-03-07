
#FINISHED
cat("\f")

values <- c(0.6248, 0.6237, 0.6118, 0.6159, 0.6298, 0.6192,
            0.6520, 0.6368, 0.6220, 0.6151, 0.6121, 0.6548,
            0.6226, 0.6280, 0.6096, 0.6300, 0.6107, 0.6392,
            0.6230, 0.6131, 0.6223, 0.6297, 0.6435, 0.5978,
            0.6351, 0.6275, 0.6261, 0.6262, 0.6262, 0.6314,
            0.6128, 0.6403, 0.6521, 0.6049, 0.6170,
            0.6134, 0.6310, 0.6065, 0.6214, 0.6141)

n <- length(values)

std <- sd(values) * sqrt((n - 1) / n)

xBar <- mean(values)

alpha <-  0.01
alphaHalf <- alpha / 2

tVal <- qt(alphaHalf, n-1, lower.tail = FALSE)

# part a)
lowerBoundCI <-  xBar - (tVal * std / sqrt(n))
upperBoundCI <- xBar + (tVal * std / sqrt(n))

# part b)
lowerBoundPI <- xBar - (tVal * std * sqrt(1 + 1 / n))
upperBoundPI <- xBar + (tVal * std * sqrt(1 + 1 / n))

kVal <- k_factor_normal(n, p <-  0.99, conf <-  0.95)

# part c)
lowerBoundTI <- xBar - (kVal * std)
upperBoundTI <- xBar + (kVal * std)

cat("a) Confidence Interval: ", lowerBoundCI, " <= u <= ", upperBoundCI, "\n")
cat("b) Prediction Interval: ", lowerBoundPI, " <= u <= ", upperBoundPI, "\n")

cat("c) Tolerance Interval: ", lowerBoundTI, ", ", upperBoundTI)