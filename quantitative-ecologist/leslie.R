M<- matrix(c(0, 0.9, 0, 0, 0, 0, 0, 0.9, 0, 0, 0, 0, 0, 0.9, 0, 0, 0, 0, 0, 0.9, 0.315, 0, 0, 0, 0.9), 5, 5)
M

det(M - diag(5)) # if non-zero, model has a unique equilibrium

e<- eigen(M) 
e
de<- e$values[1] # dominant eigenvalue is first
abs(dom)

dv<- e$vectors[,1] # dominant eigenvector is first
dv

round(abs(100*dv/sum(dv)), 0) # obtain stable age structure

Im(dom) == 0 # TRUE if monotonic, FALSE if oscillatory

