##This is a script for testing out some preliminary ideas

##Using the data generation idea from Romano et al, Achieving Equalized odds

n = 1000
library(dplyr)

A = sample(c(0,1), size = n, replace = T, prob = c(0.1,0.9))
Z1 = rnorm(n)
Z2 = rnorm(n)
X = matrix(nrow = n, ncol = 2)
Y = vector()
for(i in 1:n)
{
  X[i,] = if_else(rep(A[i] ==0, 2), c(Z1[i], 3*Z2[i]), c(3*Z1[i], Z2[i]))
  Y[i] = ifelse(A[i] == 0, 9*Z2[i] + rnorm(1), 9*Z1[i] + rnorm(1))
  
}



obj = lm(Y ~ X)
Y_hat = fitted.values(obj)
RMSE_all = sqrt(mean((Y_hat - Y)^2))
RMSE_1 = sqrt(sum(((Y_hat - Y)^2) *A)/sum(A))
RMSE_0 = sqrt(sum(((Y_hat - Y)^2) *(1 - A))/sum(1 - A))
