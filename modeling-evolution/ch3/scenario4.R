# Assumptions
# 1. Population has 2 age classes
# 2. Fecundity increases with reproductive effort
# 3. Survival decreases with reproductive effort
# 4. Fecundity is a negative function of population size


rm(list=ls())
par(mfrow=c(2,2))

# density dependence function
dd = function(alpha, beta, f.di, ei, n) {
    ei * f.di * alpha*exp(-beta*n)
}

pop.dynamics = function(e) {
    e.resident <- e[1]
    e.invader <- e[2]
    f.di <- c(4.0, 10.0)
    s.di <- c(0.6, 0.85)
    alpha <- 1; beta <- 2*10^-5; z <- 6
    
    resident.mat <- invader.mat <- matrix(0, 2, 2)
    resident.mat[1,] <- e.resident * f.di
    invader.mat[1,] <- e.invader * f.di
    # apply reproductive effort to F
    resident.mat[1,] <- e.resident * f.di
    invader.mat[1,] <- e.invader * f.di
    # apply reproductive effort to S
    resident.mat[2,] <- (1 - e.resident^z) * s.di
    invader.mat[2,] <- (1 - e.invader^z) * s.di

    maxgen1 <- 20
    n.resident <- c(1, 0)
    # resident only period
    for(i in 2:maxgen1) {
        n <- sum(n.resident)
        resident.mat[1,] <- dd(alpha, beta, f.di, e.resident, n)
        n.resident <- resident.mat %*% n.resident
    }
    # introduce invader
    maxgen2 <- 100
    pop.invader <- matrix(0, maxgen2, 1)
    pop.invader[1,1] <- 1
    n.invader <- c(1, 0)
    for(i in 2:maxgen2) {
        n <- sum(n.resident) + sum(n.invader)
        resident.mat[1,] <- dd(alpha, beta, f.di, e.resident, n)
        invader.mat[1,] <- dd(alpha, beta, f.di, e.invader, n)
        n.resident <- resident.mat %*% n.resident
        n.invader <- invader.mat %*% n.invader
        pop.invader[i] <- sum(n.invader)
    }

    generation <- seq(from=1, to=maxgen2)

    # linear regression
    invasion.model <- lm(log(pop.invader[maxgen1:maxgen2]) ~ generation[maxgen1:maxgen2])
    # elasticity value = regression slope
    elasticity <- invasion.model$coeff[2]
    
    return(elasticity)
}


#### MAIN PROGRAM ####

# execute invasion model
N1 = 30
X.Resident = seq(from=0.2, to=0.9, length=N1)
X.Invader = X.Resident
D = expand.grid(X.Resident, X.Invader)
# generate values at combinations
Z <- apply(D, 1, pop.dynamics)
Z.Mat <- matrix(Z, N1, N1)

# contour plot
contour(X.Resident, X.Invader, Z.Mat, xlab="Resident", ylab="Invader")
optimum.point = 0.5651338
points(optimum.point, optimum.point, cex=3)


## # Elasticity analysis
## dd2 = function(alpha, n1, n2) {
##     beta <- alpha * Factor
##     n1 * alpha*exp(-beta*n2)
## }

## pop.dynamics2 = function(x, coeff) {
##     alpha.resident <- x
##     alpha.invader <- alpha.resident * coeff
##     alpha <- c(alpha.resident, alpha.invader)
##     pop.dynamics(alpha)
## }

## Min.A = 1; Max.A = 10
## Coeff = 0.995
## # calculate optimum
## Optimum = optimize(pop.dynamics2, interval=c(Min.A, Max.A), Coeff)
## Best.Alpha = Optimum$root
## print('Optimum alpha')
## print(Best.Alpha)

## # plot elasticity vs alpha
## N.Int = 30
## Alpha = matrix(seq(from=Min.A, to=Max.A, length=N.Int), N.Int, 1)
## Elasticity = apply(Alpha, 1, pop.dynamics2, Coeff)
## plot(Alpha, Elasticity, type='l')
## lines(c(Min.A, Max.A), c(0, 0))

## Coeffs = Alpha / Best.Alpha
## Invasion.Exp = matrix(0, N.Int, 1) # allocate matrix
## # calculate invasion coefficient
## for(i in 1:N.Int) {
##     Invasion.Exp[i] = pop.dynamics2(Best.Alpha, Coeffs[i])
## }

## plot(Alpha, Invasion.Exp, type='l')
## points(Best.Alpha, 0, cex=2)
## # plot N(t+1) on N(t) for optimum alpha
## Max.N = 1000
## N.t = seq(from=1, to=Max.N)
## N.tplus1 = matrix(0, Max.N)
## for(i in 1:Max.N) {
##     N.tplus1[i] = dd2(Best.Alpha, N.t[i], N.t[i])
## }
## plot(N.t, N.tplus1, type='l', xlab='N(t)', ylab='N(t+1)')
## # plot N(t) on t
## Max.N = 100
## N = matrix(1, Max.N)
## for(i in 2:Max.N) {
##     N[i] = dd2(Best.Alpha, N[i-1], N[i-1])
## }
## plot(seq(from=1, to=Max.N), N, type='l', xlab='Generation', ylab='N(t)')
