# using Fisherian optimization
rm(list=ls())
par(mfrow=c(3,4))

dd = function(n, alpha, beta) {
    alpha*exp(-beta*n)
}


#### MAIN PROGRAM ####
A = c(2, 10, 20)
Factor = 0.001
N.t = seq(from=0, to=1000)
Maxgen = 100
Generation = seq(1, Maxgen)
for(j in 1:3) {
    Alpha = A[j]
    Beta = Alpha * Factor
    # first plot 
    N = length(N.t)
    N.tplus1 = matrix(0, N)
    for(i in 1:N) {
        N.tplus1[i] = N.t[i] * dd(N.t[i], Alpha, Beta)
    }
    plot(N.t, N.tplus1, type='l', xlab='N(t)', ylab='N(t+1)')
    lines(N.t, N.t)
    # second plot
    N = matrix(0, Maxgen)
    N[1] = 1
    for(i in 2:Maxgen) {
        N[i] = N[i-1] * dd(N[i-1], Alpha, Beta)
    }
    plot(Generation, N, type='l')
}    

Maxgen1 = 50
Maxgen2 = 300
Tot.Gen = Maxgen1 + Maxgen2
Generation = seq(from=1, to=Tot.Gen)
N.Start = 10 + Maxgen1
pop.dynamics = function(alpha) {
    alpha.resident <- alpha[1]
    alpha.invader <- alpha[2]
    n.resident <- n.invader <- matrix(0, Tot.Gen)
    n.resident[1] <- 1
    n.invader[Maxgen1] <- 1
    # resident only period
    for(i in 2:Maxgen1) {
        n.resident[i] <- dd(alpha.resident, n.resident[i-1], n.resident[i-1])
    }
    # introduce invader
    j <- Maxgen1 + 1
    for(i in j:Tot.Gen) {
        n.total <- n.resident[i-1] + n.invader[i-1]
        n.resident[i] <- dd(alpha.resident, n.resident[i-1], n.total)
        n.invader[i] <- dd(alpha.invader, n.invader[i-1], n.total)
    }

    # linear regression
    invasion.model <- lm(log(n.invader[N.Start:Tot.Gen]) ~ Generation[N.Start:Tot.Gen])
    # elasticity value = regression slope
    elasticity <- invasion.model$coeff[2]
    
    return(elasticity)
}

# execute invasion model
N1 = 30
X.Resident <- seq(from=2, to=4, length=N1)
X.Invader <- X.Resident
D <- expand.grid(X.Resident, X.Invader)
# generate values at combinations
Z <- apply(D, 1, pop.dynamics)
Z.Mat <- matrix(Z, N1, N1)

# contour plot
contour(X.Resident, X.Invader, Z.Mat, xlab="Resident", ylab="Invader")
points(2.725109, 2.72, cex=3)


# Elasticity analysis
pop.dynamics2 = function(x, coeff) {
    pop.dynamics(c(x, coeff))
}

Increments = 20
Coeff = 0.995
X = matrix(seq(from=0.5, to=3.0, length=Increments))
Elasticity = apply(X, 1, pop.dynamics2, Coeff)

plot(X, Elasticity, type='l')
lines(c(0.5, 3.0), c(0, 0))

# calculate optimum
Optimum = optimize(pop.dynamics2, interval=c(0.5, 3.0), Coeff)
## Optimum = uniroot(pop.dynamics2, interval=c(1.5, 3.0), Coeff)
Best.X = Optimum$minimum
## Best.X = Optimum$root
print('Optimum body size')
print(signif(Best.X, 6))

## Best.X = 1.68703 # don't know why uniroot is not giving correct value
Coeffs = X / Best.X
Invasion.Exp = matrix(0, Increments, 1) # allocate matrix

for(i in 1:Increments) {
    Invasion.Exp[i] = pop.dynamics2(Best.X, Coeff[i])
}

plot(X, Invasion.Exp, type='l')
points(Best.X, 0, cex=2)

