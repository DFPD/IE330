
#FINISHED
cat("\f")

data <- c(2.4, 2.5, 1.7, 1.6, 1.9, 2.6, 1.3, 1.9, 2.0, 2.5, 2.6,
          2.3, 2.0, 1.8, 1.3, 1.7, 2.0, 1.9, 2.3, 1.9, 2.4, 1.6)

n <- length(data)

differences <- mapply('-', data, 2.5, SIMPLIFY = FALSE)

rPlus <- 0 # counter for number of positive differences

for (i in 1 : n) { # loop through 'differences' and count positive differences
  if (differences[[i]] > 0) {
    rPlus <- rPlus + 1 # increments rPlus
  }
}

pVal <- 0

for(r in 1 : 2) {
  pVal <- pVal + choose(22, r) * (0.5)^r * (0.5)^(22-r)
}
