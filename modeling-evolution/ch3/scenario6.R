# Assumptions
# 1. Population size is governed by both density dependent and independent factors.
# 2. The density dependent components of fecundity and survival are negatively related.
# 3. The above function generates complex population dynamics.


rm(list=ls())
par(mfrow=c(5,3))

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


# Invasibility 3
dd3 = function(alpha, n1, n2) {
    #same as previous
    dd(alpha, n1, n2)
}

pop.dynamics3 = function(x, coeff) {
    a.resident <- x
    a.invader <- x * coeff
    maxgen1 <- 50
    maxgen2 <- 300
    tot.gen <- maxgen1 + maxgen2

    n.resident <- n.invader <- matrix(0, tot.gen)
    n.resident[1] <- 1
    n.invader[maxgen1] <-1

    # resident generations
    for(i in 2:maxgen1) {
        n <- n.resident[i-1]
        n.resident[i] <- dd3(a.resident, n, n) # density effect
    }

    # introduce invader
    for(i in (maxgen1+1):tot.gen) {
        n <- n.resident[i-1] + n.invader[i-1]
        n.resident[i] <- dd3(a.resident, n.resident[i-1], n)
        n.invader[i] <- dd3(a.invader, n.invader[i-1], n)
    }

    generation <- seq(from=1, to=tot.gen)

    plot(generation, n.resident, xlab='Generation', ylab='Resident N', type='l')
    plot(generation, n.invader, xlab='Generation', ylab='Invader N', type='l')
}    

Invader.Alpha = 10
pop.dynamics3(Best.Alpha, Invader.Alpha/Best.Alpha)


# Multiple invasibility analysis
dd4 = function(x, n.tot) {
    alpha <- x[1]
    n <- x[2]
    dd(alpha, n, n.tot)
}

set.seed(10)
Maxgen = 10^4
Stats = matrix(0, Maxgen, 3)
Max.Alpha = 50
N.Inc = 50
Store = matrix(0, Maxgen, N.Inc)
Data = matrix(0, N.Inc, 2)
Data[24,2] = 1
Alpha = matrix(seq(from=1, to=Max.Alpha, length=N.Inc), N.Inc, 1)
Data[,1] = Alpha
for(i in 1:Maxgen) {
    N.Total = sum(Data[,2])
    Data[,2] = apply(Data, 1, dd4, N.Total)
    Store[i,] = Data[,2]
    S = sum(Data[,2])
    Stats[i,1] = S
    Stats[i,2] = sum(Data[,1] * Data[,2]) / S # mean
    SX1 = sum(Data[,1]^2 * Data[,2])
    SX2 = (sum(Data[,1] * Data[,2]))^2 / S
    Stats[i,3] = sqrt((SX1 - SX2) / (S - 1))
    # introduce random mutant
    Mutant = ceiling(runif(1, min=0, max=50))
    Data[Mutant, 2] = Data[Mutant, 2] + 1
}

# select rows to be plotted
for(row in (Maxgen-2):Maxgen) {
    plot(Alpha, Store[row,], type='l', xlab='Alpha', ylab='N')
}    

Generation = seq(from=1, to=Maxgen)
N0 = Maxgen - 100
plot(Generation[N0:Maxgen], Stats[N0:Maxgen,1], xlab='Generation', ylab='Population', type='l')
plot(Generation[N0:Maxgen], Stats[N0:Maxgen,2], xlab='Generation', ylab='Mean', type='l')
plot(Generation[N0:Maxgen], Stats[N0:Maxgen,3], xlab='Generation', ylab='SD', type='l')
print(c('Mean alpha in last gen = ', Stats[Maxgen, 2]))
print(c('SD of alpha in last gen = ', Stats[Maxgen, 3]))
