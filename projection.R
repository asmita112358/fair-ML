##This is a script for testing out some preliminary ideas

##Using the data generation idea from Romano et al, Achieving Equalized odds


library(dplyr)
library(ks)
library(kernlab)
n = 1000
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




##Generating dummy A
y1 = Y[A == 1]
y0 = Y[A == 0]
p1 = length(y1)/n
p0 = length(y0)/n
#sd1 = sqrt(var(y1))
p = (kde(y1, eval.points = Y)$estimate*p1)/(kde(y0, eval.points = Y)$estimate*p0+ kde(y1, eval.points = Y)$estimate*p1)
A_star = rbinom(n, 1, p)

##Our method

B = matrix(c(1,1.5,-0.2,2), nrow = 2)
X_new = matrix(nrow = n, ncol = 2)
for(i in 1:n)
{
  X_new[i,] = B%*%X[i,]
}

lambda = 0.2
u = kmmd(cbind(X_new, A, Y), cbind(X_new, A_star, Y)) 
v = -0.2*sqrt(sum((B%*%cov(X))^2))


