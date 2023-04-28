# Assumptions
# 1. Population size is governed by both density dependent and independent factors.
# 2. The density dependent components of fecundity and survival are negatively related.
# 3. The above function generates complex population dynamics.


rm(list=ls())
par(mfrow=c(3,3))

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


##### MAIN PROGRAM ####
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


# Elasticity analysis
dd2 = function(alpha, n1, n2) {
    # no change from dd
    dd(alpha, n1, n2)
}

pop.dynamics2 = function(x, coeff) {
    alpha.resident <- x
    alpha.invader <- alpha.resident * coeff
    alpha <- c(alpha.resident, alpha.invader)
    pop.dynamics(alpha)
}

Min.A = 10; Max.A = 40
Coeff = 0.995
# calculate optimum
Optimum = uniroot(pop.dynamics2, interval=c(Min.A, Max.A), Coeff)
Best.Alpha = Optimum$root
print('Optimum reproductive effort')
print(Best.Alpha)

# plot elasticity vs alpha
N.Int = 30
Low = Min.A
High = Max.A
Alpha = matrix(seq(from=Low, to=High, length=N.Int), N.Int, 1)
Elasticity = apply(Alpha, 1, pop.dynamics2, Coeff)
plot(Alpha, Elasticity, type='l')
lines(c(Low, High), c(0, 0))

Coeffs = Alpha / Best.Alpha
Invasion.Exp = matrix(0, N.Int, 1) # allocate matrix
# calculate invasion coefficient
for(i in 1:N.Int) {
    Invasion.Exp[i] = pop.dynamics2(Best.Alpha, Coeffs[i])
}

plot(Alpha, Invasion.Exp, type='l')
points(Best.Alpha, 0, cex=2)

# plot N(t+1) vs N(t)
N0 = 1
N = 1000
N.t = seq(from=N0, to=N)
N.tplus1 = matrix(0, N)
for(i in N0:N) {
    N.tplus1[i] = dd2(Best.Alpha, N.t[i], N.t[i])
}
plot(N.t, N.tplus1, type='l')

# plot N(t) vs t
N0 = 1
N = 100

N.t = matrix(0, N)
N.t[1] = 1
for(i in (N0+1):N) {
    N.t[i] = dd2(Best.Alpha, N.t[i-1], N.t[i-1])
}
plot(seq(from=N0, to=N), N.t, type='l', xlab='Generation', ylab='Population')

