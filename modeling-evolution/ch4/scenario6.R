rm(list=ls())
library(MASS)

# Evolution of two traits using an Individual Variance Components model

# General assumptions
# 1. Generations are non-overlapping
# 2. There is a negative correlation (genetic and phenotypic) between the two traits X, Y
# 3. Fitness is equal to the value of trait X for X > 0.
# 4. If trait X is negative, fitness is zero. A simple example is if the realized manifestation of trait X is itself fecundity.


selection = function(trait.p, trait.a) {
    fec <- trait.p[,1]
    fec[trait.p[,1] < 0] <- 0
    fec.tot <- sum(fec)
    new.x1 <- sum(fec * trait.a[,1]) / fec.tot
    new.x2 <- sum(fec * trait.a[,2]) / fec.tot
    mean.a <- c(new.x1, new.x2)
    return(mean.a)
}

#### MAIN PROGRAM ####
set.seed(100)

N = 1000
Maxgen = 10
Output = matrix(0, Maxgen, 3)
NX = 2 # number of traits
# create initial matrices
# sexes are ignored
H2 = matrix(c(0.4, -0.8, -0.7, 0.5), 2, 2, byrow=T) # heritability
Mean.A = c(3, 3)
Mean.E = c(0, 0)
Vp = c(1.0, 0.5) # phenotypic variance
# phenotypic  covariance matrix (initial value of 1 is arbitrary)
Cov.P = matrix(1, NX, NX)
diag(Cov.P) = Vp
Cov.A = matrix(0, NX, NX)
Cov.E = matrix(0, NX, NX)
for(i in 1:NX) {
    Cov.A[i,i] = Cov.P[i,i] * H2[i,i]
    Cov.E[i,i] = Cov.P[i,i] - Cov.A[i,i]
    if(Cov.E[i,i] < 0) stop(print(c('Cov.E cannot be', i, Cov.E[i,i])))
}    

# phenotypic and genetic covariances
for(i in 1:(NX-1)) {
    jj = i + 1
    for(j in jj:NX) {
        Cov.P[j,i] = Cov.P[i,j]
        Cov.A[i,j] = H2[i,j] * sqrt(Cov.A[i,i] * Cov.A[j,j])
        Cov.A[j,i] = Cov.A[i,j]
        Cov.E[i,j] = Cov.P[i,j] - Cov.A[i,j]
        Cov.E[j,i] = Cov.E[i,j]
    }    
}

for(i in 1:Maxgen) {
    Trait.E = mvrnorm(N, mu=Mean.E, Sigma=Cov.E)
    Trait.A = mvrnorm(N, mu=Mean.A, Sigma=Cov.A)
    Trait.P = Trait.A + Trait.E
    Output[i,1] = i
    Output[i,2] = mean(Trait.P[,1])
    Output[i,3] = mean(Trait.P[,2])
    Mean.A = selection(Trait.P, Trait.A)
}    

# graphing parameters
par(mfrow=c(1,1))
Y.Min = min(Output[,2:3])
Y.Max = max(Output[,2:3])
plot(Output[,1], Output[,2], type='l', ylim=c(Y.Min, Y.Max), xlab='Generation', ylab='Trait X (solid), Trait Y (dotted)')
lines(Output[,1], Output[,3], lty=2) # Trait Y
