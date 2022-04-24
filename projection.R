##This is a script for testing out some preliminary ideas

##Using the data generation idea from Romano et al, Achieving Equalized odds

n = 1000


A = sample(c(0,1), size = n, replace = T, prob = c(0.1,0.9))
n1 = sum(A == 1)
n0 = sum(A == 0)
e1 = rnorm(n1)
e0 = rnorm(n0)
Z1 = rnorm(n1)
Z2 = rnorm(n0)
Y0 = 9*Z2 + e0
Y1 = 9*Z1 + e1


A_new = c(A[A==0], A[A==1])
data = data.frame(Y, A_new,)


b