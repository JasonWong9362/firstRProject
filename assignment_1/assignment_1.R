#1
animals <- c("Snake", "Ostrich", "Cat", "Spider")
num_legs <- c(0, 2, 4, 8)
animals_legs_df <- data.frame(animals, num_legs)

#2
#  starting at 12 and decreasing to 2 in steps of -2
x_vect <- seq(12, 2, by = -2)
# convert the vector x_vect into a matrix with 2 rows and 3 coloumns
X <- matrix(x_vect, nrow = 2, ncol = 3)
# create a 2 by 2 matrix called Y consisting of sequence of four numbers from 1-4
Y <- matrix(seq(1,4), nrow = 2, ncol = 2)
Z <- matrix(seq(4,10, by = 2), nrow = 2, ncol = 2)
# matrix transpose
Y_t = t(Y)
Z_t = t(Z)
# matrix addition
YZ_Sum = Y + Z
ZY_Sum = Z + Y
# matrix multiplication
YZ_product = Y%*%Z
ZY_product = Z%*%Y
# matrix element-wise multiplication
YZ_element_wise_product = Y*Z
ZY_element_wise_product = Z*Y
# matrix product YX
YX_product = Y%*%X
# matrix product XY
# XY_product = X%*%Y 
# Error in X %*% Y : non-conformable arguments
# inverse matrix Y^-1: solve(Y)
iY = solve(Y)
# matrix product Y^-1Y 
# the result of Y^-1Y should be unit matrix I
iYY_product = iY%*%Y
# matrix product Y^-1X
iYX_product = iY%*%X





