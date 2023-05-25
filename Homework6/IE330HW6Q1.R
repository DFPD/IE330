# FINISHED
rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\f")

a <- 4
fknot <- 4.42
MSFactor <- 330.4716
DFTotal <- 31

replicates <-  n <- (DFTotal+1)/ 4
DFFactor <- a-1
DFError <- a*(n-1)
SSFactor <- MSFactor*(a-1)
MSError <- MSFactor / fknot
SSE <- MSError*(a*(n-1))
SST <- SSFactor+SSE
pValue <- pf(4.42, 3, 28, lower.tail = FALSE)

cat("Look through the environment and all the variables for",
    "ANOVA have been calculated")
