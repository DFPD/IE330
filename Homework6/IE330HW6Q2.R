# UNFINISHED
rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\f")

data <- matrix(c(2.7, 4.6, 2.6, 3.0, 3.2, 3.8,
                 4.9, 4.6, 5.0, 4.2, 3.6, 4.2,
                 4.6, 3.4, 2.9, 3.5, 4.1, 5.1),  ncol = 6, byrow = TRUE)
colnames(data) <- c("1", "2", "3", "4", "5", "6")
rownames(data) <- c("125", "160", "200")
# data <- as.table(data)

a <- 3
n <- 6

sumrow1 <- sum(data[1, 1:6])
sumrow2 <- sum(data[2, 1:6])
sumrow3 <- sum(data[3, 1:6])
sumtable <- c(sumrow1, sumrow2, sumrow3) # table of the sums of each row
avgtable <- sumtable/6 # table of the averages of the sum of each row

total <- sum(sumtable) # sum of all data
avgTotal <- total/(n*a) # average of the sum of all data

SSFlow <- 0
for(i in 1 : a) {
  SSFlow <- SSFlow + (avgtable[i] - avgTotal)^2 #IMPORTANT: WHY TF IT WRONG?!!
}
SSFlow <- SSFlow * n

SSE <- 0
for(i in 1 : a) {
  for (j in 1 : n) {
    SSE <- SSE + (data[i, j] - avgtable[i])^2
  }
}

SST <- 0
for(i in 1 : a) {
  for (j in 1 : n) {
    SST <- SST + (data[i, j] - avgTotal)^2
  }
}

MSFlow <- SSFlow / (a - 1)
MSError <- SSE / (a*(n-1))

Fknot <- MSFlow/MSError


