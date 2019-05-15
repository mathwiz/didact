A<- matrix(c(1, .01, .17, 1, .012, .2136, 1, .016, .25281), 3, 3)
b<- matrix(c(1905, 20, 338), 3, 1)

det(A)
solve(A)
solve(A) %*% b
solve(A, b)
