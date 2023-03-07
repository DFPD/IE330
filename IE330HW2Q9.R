
#FINISHED
cat("\f")

sample <- c( 7.91, 7.85, 6.82, 8.01, 7.46, 6.95, 7.05, 7.35, 7.25, 7.42)

n <- length(sample)

u <- 7

differences <- mapply('-', sample, 7, SIMPLIFY = FALSE)

rPlus <- 0 # counter for number of positive differences

for (i in 1 : n) { # loop through 'differences' and count positive differences
  if (differences[[i]] > 0) {
    rPlus <- rPlus + 1 # increments rPlus
  }
}

pVal <- 0

for(r in 8 : 10) {
  pVal <- pVal + choose(10, r) * (0.5)^r * (0.5)^(10-r)
}

pVal <- 2 * pVal

  