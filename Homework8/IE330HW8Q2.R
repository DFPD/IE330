# FINISHED
rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\f")

m <- 20
n <- 5

A2 <- 0.577
D3 <- 0
D4 <- 2.115
C4 <- 0.94

x1 <- c(4.960, 4.958, 4.971, 4.940, 4.964, 4.969, 4.960, 4.969, 4.984, 4.970,
        4.975, 4.945, 4.976, 4.970, 4.982, 4.961, 4.980, 4.975, 4.977, 4.975)
x2 <- c(4.946, 4.927, 4.929, 4.982, 4.950, 4.951, 4.944, 4.949, 4.928, 4.934,
        4.959, 4.977, 4.964, 4.954, 4.962, 4.943, 4.970, 4.968, 4.966, 4.967)
x3 <- c(4.950, 4.935, 4.965, 4.970, 4.953, 4.955, 4.957, 4.963, 4.960, 4.961,
        4.962, 4.950, 4.970, 4.964, 4.968, 4.950, 4.975, 4.971, 4.969, 4.969)
x4 <- c(4.956, 4.940, 4.952, 4.953, 4.962, 4.966, 4.948, 4.952, 4.943, 4.940,
        4.971, 4.969, 4.968, 4.959, 4.975, 4.949, 4.978, 4.969, 4.973, 4.972)
x5 <- c(4.958, 4.950, 4.938, 4.960, 4.956, 4.954, 4.951, 4.962, 4.955, 4.965,
        4.968, 4.954, 4.972, 4.968, 4.963, 4.957, 4.977, 4.972, 4.970, 4.972)

Combined_Groups <- data.frame(cbind(x1, x2, x3, x4, x5))

means <- c()
for(i in 1 : m) {
  means <- c(means, rowMeans(Combined_Groups)[i]) # vector storing the means of each row
}
sumMean <- sum(means) # taking the sum of the means of each row
grandMean <- (1/m) * sumMean


ranges <- c()
for(i in 1 : m) {
  ranges = c(ranges, diff(range(Combined_Groups[i, ])) )
}
sumRange <- sum(ranges)
avgRange <- (1/m) * sumRange


stds <- c()
for(i in 1 : m) {
  stds <- c(stds, sd(Combined_Groups[i, ])  )
}
sumStds <- sum(stds)
avgStd <- (1/m) * sumStds


# --------- X Bar Chart
xrUCL <- grandMean + A2*avgRange
xrCL <- grandMean
xrLCL <- grandMean - A2*avgRange


# -------- R Chart
rUCL <- D4*avgRange
rCL <- avgRange
rLCL <- D3*avgRange


#-------- S Chart
xsUCL <- grandMean + (3 * avgStd)/(C4*sqrt(n))
xsCL <- grandMean
xsLCL <- grandMean - (3 * avgStd)/(C4*sqrt(n))

sUCL <- avgStd + 3*(avgStd/C4)*(sqrt(1 - C4^2))
sCL <- avgStd
sLCL <- avgStd - 3*(avgStd/C4)*(sqrt(1 - C4^2))


# xBar/R Charts
plot(x = c(1:20), y = means, type = 'b', xlab = "m", ylab = "Sample Mean")
abline(h = xrUCL, col = "red", lwd = 2)
abline(h = xrCL, col = "green", lwd = 2)
abline(h = xrLCL, col = "red", lwd = 2)

plot(x = c(1:20), y = ranges, type = 'b', xlab = "m", ylab = "Sample Range")
abline(h = rUCL, col = "red", lwd = 2)
abline(h = rCL, col = "green", lwd = 2)
abline(h = rLCL, col = "red", lwd = 2)


# xBar/S Charts
plot(x = c(1:20), y = means, type = 'b', xlab = "m", ylab = "Sample Mean")
abline(h = xsUCL, col = "red", lwd = 2)
abline(h = xsCL, col = "green", lwd = 2)
abline(h = xsLCL, col = "red", lwd = 2)

plot(x = c(1:20), y = stds, type = 'b', xlab = "m", ylab = "Sample stDev")
abline(h = sUCL, col = "red", lwd=2)
abline(h = sCL, col = "green", lwd=2)
abline(h = sLCL, col = "red", lwd=2)

