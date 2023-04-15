# using Fisherian optimization
rm(list=ls())

# using R0 as a fitness measure
rcalc = function(x) {
    af <- 0; bf <- 16; as <- 1; bs <- 0.5
    a <- af + bf*x
    b <- as + bs*x
    r0 <- a*exp(-b) / (1 - exp(-b))
    return(r0)
}

Result = optimize(f=rcalc, interval=c(0.1, 3), maximum=T)$maximum
print('Calculated')
print(Result)


# using Leslie matrix
## rm(list=ls())

leslie = function(x, maxage) {
    age <- seq(from=1, to=maxage)
    af <- 0; bf <- 16; as <- 1; bs <- 0.5
    m <- rep(af + bf*x, times=maxage)
    surv <- exp(-(as + bs*x) * age)
    age.surv <- matrix(0, maxage, 1)
    age.surv[1] = surv[1]
    for(i in 2:(maxage-1)) {
        age.surv[i] <- surv[i] / surv[i-1]
    }
    fertility <- m * age.surv # top row of leslie mat
    dummy <- matrix(0, maxage-1, maxage-1)
    diag(dummy) <- age.surv[1:(maxage-1)]
    leslie.mat <- matrix(0, maxage, maxage)
    leslie.mat[1,] <- fertility
    leslie.mat[2:maxage, 1:(maxage-1)] <- dummy
    return(leslie.mat)
}

dd = function(alpha, beta, fi, n) {
    fi*alpha*exp(-beta*n)
}

pop.dynamics = function(x) {
    x.resident <- x[1]
    x.invader <- x[2]
    alpha <- 1.0; beta <- 2*10^-5
    maxage <- 50
    resident.mat <- leslie(x.resident, maxage)
    invader.mat <- leslie(x.invader, maxage)
    f.resident <- resident.mat[1,]
    f.invader <- invader.mat[1,]
    maxgen <- 30
    n.resident <- matrix(0, maxage, 1) # pop size
    n.resident[1] <- 1
    for(i in 2:maxgen) {
        n <- sum(n.resident)
        resident.mat[1,] <- dd(alpha, beta, f.resident, n)
        n.resident <- resident.mat %*% n.resident
    }
    # introduce invader
    maxgen <- 100
    pop.invader <- matrix(0, maxgen, 1)
    n.invader <- matrix(0, maxage, 1)
    n.invader[1] <- 1
    pop.invader[1,1] <- n.invader[1]
    for(i in 2:maxgen) {
        n <- sum(n.resident) + sum(n.invader)
        resident.mat[1,] <- dd(alpha, beta, f.resident, n)
        n.resident <- resident.mat %*% n.resident
        invader.mat[1,] <- dd(alpha, beta, f.invader, n)
        n.invader <- invader.mat %*% n.invader
        pop.invader[i] <- sum(n.invader)
    }

    generation <- seq(from=1, to=maxgen)
    n.start <- 20
    # linear regression
    invasion.model <- lm(log(pop.invader[n.start:maxgen]) ~ generation[n.start:maxgen])
    # elasticity value = regression slope
    elasticity <- invasion.model$coeff[2]
    
    return(elasticity)
}

# execute Leslie
N1 = 30
X.Resident <- seq(from=1, to=3, length=N1)
X.Invader <- X.Resident
D <- expand.grid(X.Resident, X.Invader)
# generate values at combinations
Z <- apply(D, 1, pop.dynamics)
Z.Mat <- matrix(Z, N1, N1)

# contour plot
par(mfrow=c(2,2))
contour(X.Resident, X.Invader, Z.Mat, xlab="Resident", ylab="Invader")
points(1.68703, 1.68703, cex=3)


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

