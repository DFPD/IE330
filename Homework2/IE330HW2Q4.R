
#FINISHED
cat("\f")

readings <- c(2.69, 5.76, 2.67, 1.62, 4.12)

n <- length(readings)
std <- 0.66
xBar <- mean(readings)

zKnot <- (xBar - 2.5) / (std / sqrt(n))

alpha <- 0.05

zCritHigh <- qnorm(alpha, lower.tail = FALSE)

pValue <- pnorm(zKnot, lower.tail = FALSE)
