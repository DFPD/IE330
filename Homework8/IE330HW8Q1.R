# FINISHED
rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\f")

m <- 30
n <- 5

sumAvg <- 140.03
sumRange <- 13.63
sumStd <- 5.10

A2 <- 0.577
D3 <- 0
D4 <- 2.115
C4 <- 0.94

grandMean <- (1/m)*sumAvg
avgRange <- (1/m)*sumRange
avgStd <- (1/m)*sumStd

xrUCL <- grandMean + A2*avgRange
xrCL <- grandMean
xrLCL <- grandMean - A2*avgRange

rUCL <- D4*avgRange
rCL <- avgRange
rLCL <- D3*avgRange

xsUCL <- grandMean + (3 * avgStd)/(C4*sqrt(n))
xsCL <- grandMean
xsLCL <- grandMean - (3 * avgStd)/(C4*sqrt(n))

sUCL <- avgStd + 3*(avgStd/C4)*(sqrt(1 - C4^2))
sCL <- avgStd
sLCL <- avgStd - 3*(avgStd/C4)*(sqrt(1 - C4^2))


