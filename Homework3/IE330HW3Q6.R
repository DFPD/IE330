
# FINISHED
cat("\f")

n1 <- 8
xBar1 <- 90
std1 <- 5

n2 <- 9
xBar2 <- 115
std2 <- 10

CI <- 95
alpha <- (100 - CI) / 100
alphaHalf <-  alpha / 2

tKnot <- (xBar1 - xBar2) / sqrt((std1^2 / n1) + (std2^2 / n2))

degFree <- floor (   ((std1^2 / n1) + (std2^2 / n2))^2 / ( 
  (((std1^2 / n1)^2) / (n1 - 1)) + (((std2^2 / n2)^2) / (n2 - 1)) )   )
# rounds down the degrees of freedom to the nearest integer

tCritBottom <- qt(alpha, degFree, lower.tail = TRUE)
tCritTop <- qt(alpha, degFree, lower.tail = FALSE)

pVal <- pt(tKnot, degFree, lower.tail = TRUE)

CIUpperBound <- xBar1 - xBar2 + tCritTop *
  sqrt( (std1^2 / n1) + (std2^2 / n2) )

if (tKnot < tCritBottom) {
  writeLines("Part a) since tKnot < tCritBottom, we reject H0")
} else {
  writeLines("Part a) since tKnot > tCritBottom, we fail to reject H0")
}

cat("Part b) the p-value is", pVal, "\n")

writeLines("Part c) Here is your 1-sided upper confidence integral:")
cat("u1 - u2 <= ", CIUpperBound, "mmHg\n")

tKnotPB <- (xBar1 - xBar2 + 15) / sqrt( (std1^2 / n1) + (std2^2 / n2) )
# tKnot for part d

if (tKnotPB < tCritBottom) {
  writeLines("Part d) since tKnot < tCritBottom, we reject H0")
} else {
  writeLines("Part d) since tKnot > tCritBottom, we reject H0")
}

cat("Part e) Since -15 is not within the confidence interval, we reject H0")
