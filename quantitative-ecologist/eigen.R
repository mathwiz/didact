M<- matrix(c(0.77, 0.33, 0.11, 0.99), 2, 2)
M
eigen(M)

## standardize the eigenvectors
n<- eigen(M)$vectors
n
st<- diag(1/n[1,])
st
n %*% st

