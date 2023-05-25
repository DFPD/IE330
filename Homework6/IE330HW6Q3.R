# UNFINISHED
rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\f")


data <- matrix(c(14.8, 14.8, 14.7, 14.8, 14.9,
                 14.6, 15.0, 14.9, 14.8, 14.7,
                 12.7, 11.6, 12.4, 12.7, 12.1,
                 14.2, 14.4, 14.4, 12.2, 11.7), ncol = 5, byrow = TRUE)
rownames(data) <- c("1", "2", "3", "4")

a <- 4
n <- 5

sumrow1 <- sum(data[1, 1:5])
sumrow2 <- sum(data[2, 1:5])
sumrow3 <- sum(data[3, 1:5])
sumrow4 <- sum(data[4, 1:5])
sumtable <- t(c(sumrow1, sumrow2, sumrow3, sumrow4)) # table of the sums of each row
avgtable <- sumtable/5 # table of the averages of the sum of each row


total <- sum(sumtable) # sum of all data
avgTotal <- total/(n*a) # average of the sum of all data

SSMethod <- 0
for(i in 1 : a) {
  SSMethod <- SSMethod + (avgtable[i] - avgTotal)^2 #IMPORTANT: WHY TF IT WRONG?!!
  cat("Current value:", SSMethod, "\n")
}
SSMethod <- SSMethod * n


SSE <- 0
for(k in 1 : a) {
  for (j in 1 : n) {
    SSE <- SSE + (data[k, j] - avgtable[k])^2
  }
}

SST <- SSMethod + SSE

MSMethod <- SSMethod / (a - 1)
MSError <- SSE / (a*(n-1))

Fknot <- MSMethod/MSError
