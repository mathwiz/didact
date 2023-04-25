# using Fisherian optimization
rm(list=ls())
par(mfrow=c(3,3))

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
dd2 = function(alpha, n1, n2) {
    beta <- alpha * Factor
    n1 * alpha*exp(-beta*n2)
}

pop.dynamics2 = function(x, coeff) {
    alpha.resident <- x
    alpha.invader <- alpha.resident * coeff
    alpha <- c(alpha.resident, alpha.invader)
    pop.dynamics(alpha)
}

Min.A = 1; Max.A = 10
Coeff = 0.995
# calculate optimum
Optimum = optimize(pop.dynamics2, interval=c(Min.A, Max.A), Coeff)
Best.Alpha = Optimum$root
print('Optimum alpha')
print(Best.Alpha)

# plot elasticity vs alpha
N.Int = 30
Alpha = matrix(seq(from=Min.A, to=Max.A, length=N.Int), N.Int, 1)
Elasticity = apply(Alpha, 1, pop.dynamics2, Coeff)
plot(Alpha, Elasticity, type='l')
lines(c(Min.A, Max.A), c(0, 0))

Coeffs = Alpha / Best.Alpha
Invasion.Exp = matrix(0, N.Int, 1) # allocate matrix
# calculate invasion coefficient
for(i in 1:N.Int) {
    Invasion.Exp[i] = pop.dynamics2(Best.Alpha, Coeffs[i])
}

plot(Alpha, Invasion.Exp, type='l')
points(Best.Alpha, 0, cex=2)
# plot N(t+1) on N(t) for optimum alpha
Max.N = 1000
N.t = seq(from=1, to=Max.N)
N.tplus1 = matrix(0, Max.N)
for(i in 1:Max.N) {
    N.tplus1[i] = dd2(Best.Alpha, N.t[i], N.t[i])
}
plot(N.t, N.tplus1, type='l', xlab='N(t)', ylab='N(t+1)')
# plot N(t) on t
Max.N = 100
N = matrix(1, Max.N)
for(i in 2:Max.N) {
    N[i] = dd2(Best.Alpha, N[i-1], N[i-1])
}
plot(seq(from=1, to=Max.N), N, type='l', xlab='Generation', ylab='N(t)')
