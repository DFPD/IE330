# FINISHED
rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\f")

a <- 2
b <- 3
n <- 3

m1 <- c(g1 = c(74, 64, 50), g2 = c(92, 86, 68))
m2 <- c(g1 = c(73, 61, 44), g2 = c(98, 73, 88))
m3 <- c(g1 = c(78, 85, 92), g2 = c(66, 45, 85))


Combined_Groups <- data.frame(cbind(m1, m2, m3))
sectorAvgs <- data.frame(rbind( c(59.333, 59.333, 85), c(82, 86.333, 65.333)))

rowTotals <- c()
sum <- 0
for(i in 1 : 6) { # through columns
  sum <- sum + sum(Combined_Groups[i, ])
  if(i == 3 || i == 6) { # Clever way of only summing the two fat columns
    rowTotals <- c(rowTotals, sum)
    sum <- 0
  }
}
rowAvgs <- rowTotals / (b*n)


colTotals <- c()
for(i in 1 : b){
  colTotals <- c(colTotals, sum(Combined_Groups[ , i]))
}
colAvgs <- colTotals / (b*a)


total <- sum(Combined_Groups)
totalAvg <- total / (a*b*n)


SSA <- 0
for(i in 1 : a) {
  SSA = SSA + (rowAvgs[i] - totalAvg)^2
}
SSA <- b*n*SSA


SSB <- 0
for(j in 1 : b) {
  SSB = SSB + (colAvgs[j] - totalAvg)^2
}
SSB <- a*n*SSB


SSAB <- 0
for(i in 1 : a) {
  for( j in 1 :b) {
    SSAB <-  SSAB + (sectorAvgs[i, j] - rowAvgs[i] - colAvgs[j] + totalAvg)^2
    print(sectorAvgs[i,j])
    print(rowAvgs[i])
  }
}
SSAB <- n*SSAB


SSE <- 0
count <- 1
for(i in 1 : 6) { # through columns
  for(j in 1 : b) { # through rows
    SSE <- SSE + (Combined_Groups[i,j] - sectorAvgs[count, j])^2
  }
  if(i == 3) {
    count <- 2
  }
}
