# FINISHED
rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\f")

k <- 3
n <- 2
dima <- 2
dimb <- 2
dimc <- 2

one <- c(221, 311)
sumone <- sum(one)
avgOne <- one/n

a <- c(325, 435)
suma <- sum(a)
avga <- a/n

b <- c(354, 348)
sumb <- sum(b)
avgb <- b/n

ab <- c(552, 472)
sumab <- sum(ab)
avgab <- ab/n

c <- c(440, 453)
sumc <- sum(c)
avgc <- c/n

ac <- c(406, 377)
sumac <- sum(ac)
avgac <- ac/n

bc <- c(605, 500)
sumbc <- sum(bc)
avgbc <- bc/n

abc <- c(392, 419)
sumabc <- sum(abc)
avgabc <- abc/n

A <- (1/(4*n))*(suma + sumab + sumac + sumabc - sumone - sumb - sumc - sumbc)
B <- (1/(4*n))*(sumb + sumab + sumbc + sumabc - sumone - suma - sumc - sumac)
C <- (1/(4*n))*(sumc + sumac + sumbc + sumabc - sumone - suma - sumb - sumab)
AB <- (1/(4*n))*(sumabc - sumbc + sumab - sumb - sumac + sumc - suma + sumone)
AC <- (1/(4*n))*(sumone - suma + sumb - sumab - sumc + sumac - sumbc + sumabc)
BC <- (1/(4*n))*(sumone + suma - sumb - sumab - sumc - sumac + sumbc + sumabc)
ABC <- (1/(4*n))*(sumabc - sumbc - sumac + sumc - sumab + sumb + suma - sumone)


SSA <- ((suma + sumab + sumac + sumabc - sumone - sumb - sumc - sumbc)^2)/(n*2^k)
SSB <- ((sumb + sumab + sumbc + sumabc - sumone - suma - sumc - sumac)^2)/(n*2^k)
SSC <- ((sumc + sumac + sumbc + sumabc - sumone - suma - sumb - sumab)^2)/(n*2^k)
SSAB <- ((sumabc - sumbc + sumab - sumb - sumac + sumc - suma + sumone)^2)/(n*2^k)
SSAC <- ((sumone - suma + sumb - sumab - sumc + sumac - sumbc + sumabc)^2)/(n*2^k)
SSBC <- ((sumone + suma - sumb - sumab - sumc - sumac + sumbc + sumabc)^2)/(n*2^k)
SSABC <- ((sumabc - sumbc - sumac + sumc - sumab + sumb + suma - sumone)^2)/(n*2^k)

totalMean <- ((suma + sumb + sumc + sumab + sumac + sumbc + sumabc + sumone)/(n*2^k))

SST <- 
  (one[1] - totalMean)^2 + (one[2] - totalMean)^2 + (a[1] - totalMean)^2 + 
  (a[2] - totalMean)^2 + (b[1] - totalMean)^2 + (b[2] - totalMean)^2 +
  (c[1] - totalMean)^2 + (c[2] - totalMean)^2 + (ab[1] - totalMean)^2 +
  (ab[2] - totalMean)^2 + (ac[1] - totalMean)^2 + (ac[2] - totalMean)^2 +
  (bc[1] - totalMean)^2 + (bc[2] - totalMean)^2 + (abc[1] - totalMean)^2 +
  (abc[2] - totalMean)^2

SSE <- SST - (SSA + SSB + SSC + SSAB + SSAC + SSBC + SSABC)


dfA <- dima-1
dfB <- dimb-1
dfC <- dimc-1
dfAB <- (dima-1)*(dimb-1)
dfAC <- (dima-1)*(dimc-1)
dfBC <- (dimb-1)*(dimc-1)
dfABC <- (dima-1)*(dimb-1)*(dimc-1)
dfErr <- dima*dimb*dimc*(n-1)
dfTotal <- dima*dimb*dimc*n-1

MSA <- SSA/dfA
MSB <- SSB/dfB
MSC <- SSC/dfC
MSAB <- SSAB/dfAB
MSAC <- SSAC/dfAC
MSBC <- SSBC/dfBC
MSABC <- SSABC/dfABC
MSE <- SSE/dfErr

FA <- MSA/MSE
FB <- MSB/MSE
FC <- MSC/MSE
FAB <- MSAB/MSE
FAC <- MSAC/MSE
FBC <- MSBC/MSE
FABC <- MSABC/MSE


