# FINISHED
rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\f")


xx <- matrix( c(4.3873, -4.0042e-2, -4.1679e-2,
       -4.0042e-2, 6.0774e-4, -7.3875e-5,
       -4.1679e-2, -7.3875e-5, 2.5766e-4), nrow = 3, ncol = 3)

xy <- matrix( c(4757.9,
                334335.8,
                179706.7), nrow = 3, ncol = 1)

solutions <- crossprod(xx, xy)

# Part a)
b0 <- solutions[[1]]
b1 <- solutions[[2]]
b2 <- solutions[[3]]
cat("a) Multiple Linear Regression Model:
    y =", b0, "+", b1, 'x1 + ', b2, 'x2\n')

yEstimate <- b0 + b1*6*12 + b2*34
# Part b)
cat("b) Body fat of a man when x1 = 6 and x2 = 34:", yEstimate, "\n")
