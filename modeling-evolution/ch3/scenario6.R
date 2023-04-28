# Assumptions
# 1. Population size is governed by both density dependent and independent factors.
# 2. The density dependent components of fecundity and survival are negatively related.
# 3. The above function generates complex population dynamics.


rm(list=ls())
par(mfrow=c(2,2))

Ps = c(0.1, 0.5, 0.7, 0.9)
Maxgen = 100

dd = function(alpha, n1, n2) {
    a.min <- 1.0; a.max <- 50.0
    b.min <- 0.0; b.max <- 0.9
    beta <- 0.01
    a <- 20.0
    aa <- (alpha - a.min) / (a.max - a.min)
    s <- (b.max - b.min) * (1 - aa) / (1 + a * aa)
    n <- n1 * (alpha*exp(-beta*n2) + s)
    
    return(n)
}    

pop.dynamics = function(alpha) {
    a.resident <- alpha[1]
    a.invader <- alpha[2]
    maxgen1 <- 50
    maxgen2 <- 300
    tot.gen <- maxgen1 + maxgen2

    n.resident <- n.invader <- matrix(0, tot.gen)
    n.resident[1] <- 1
    n.invader[maxgen1] <-1

    # resdident generations
    for(i in 2:maxgen1) {
        n <- n.resident[i-1]
        n.resident[i] <- dd(a.resident, n, n) # density effect
    }

    # introduce invader
    for(i in (maxgen1+1):tot.gen) {
        n <- n.resident[i-1] + n.invader[i-1]
        n.resident[i] <- dd(a.resident, n.resident[i-1], n)
        n.invader[i] <- dd(a.invader, n.invader[i-1], n)
    }

    gen.start <- 10 + maxgen1
    gen.end <- tot.gen
    generation <- seq(from=1, to=gen.end)
    # linear regression
    invasion.model <- lm(log(n.invader[gen.start:gen.end]) ~ generation[gen.start:gen.end])
    # elasticity value = regression slope
    elasticity <- invasion.model$coeff[2]
    
    return(elasticity)
}


## #### MAIN PROGRAM ####
Best.Alpha = 24.21837

N.Int = 30
Low = 5
High = 45
A.Resident = A.Invader = seq(from=Low, to=High, length=N.Int)
D = expand.grid(A.Resident, A.Invader)

Z = apply(D, 1, pop.dynamics)
Z.Mat = matrix(Z, N.Int, N.Int)
contour(A.Resident, A.Invader, Z.Mat, xlab='Resident', ylab='Invader')
points(Best.Alpha, Best.Alpha, cex=2)
