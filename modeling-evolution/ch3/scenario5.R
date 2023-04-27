# Assumptions
# 1. Population consists of 2 stages with reprodution in second stage.
# 2. A proportion of first stage remains there each population cycle.
# 3. Fecundity is density dependent.


rm(list=ls())
par(mfrow=c(3,2))

Alpha = 18; Beta = 0.01; S = 0.8
Ps = c(0.1, 0.5, 0.7, 0.9)
Maxgen = 100

exponential = function(alpha, beta, stage.val) {
    alpha*exp(-beta*stage.val)
}    

for(i in 1:4) {
    P = Ps[i]
    Stage = c(0, 1) # start with one adult
    Stage.Mat = matrix(c(S*P, S*(1-P), exponential(Alpha, Beta, Stage[2]), 0), 2, 2)
    Store.Stage = matrix(0, Maxgen, 2)
    Store.Stage[1,] = Stage
    for(j in 2:Maxgen) {
        Stage = Stage.Mat %*% Stage
        Stage.Mat[1,2] = exponential(Alpha, Beta, Stage[2])
        Store.Stage[j,] = Stage
    }
    plot(seq(from=1, to=Maxgen), Store.Stage[,2], xlab='Generation', ylab='Adults', type='l')
}
    
pop.dynamics = function(p, coeff) {
    p.resident <- p
    p.invader <- p * coeff
    alpha <- 18.0; beta <- 0.01; s <- 0.8
    resident.stage <- c(1.0, 1.0)
    invader.stage <- c(1.0, 1.0)
    
    resident.mat <-
        matrix(c(s*p.resident, s*(1-p.resident), exponential(alpha, beta, resident.stage[2]), 0), 2, 2)
    invader.mat <-
        matrix(c(s*p.invader, s*(1-p.invader), exponential(alpha, beta, invader.stage[2]), 0), 2, 2)
    store.invader <- matrix(0, Maxgen, 2)
    store.invader[1,] <- invader.stage

    maxgen1 <- 100
    # resident only period
    for(i in 2:maxgen1) {
        resident.stage <- resident.mat %*% resident.stage # revise matrix
        resident.mat[1,2] <- exponential(alpha, beta, resident.stage[2]) # density effect
    }
    # introduce invader
    maxgen2 <- 100
    pop.invader <- matrix(0, maxgen2, 1)
    for(i in 1:maxgen2) {
        n <- resident.stage[2] + invader.stage[2]
        resident.mat[1,2] <- exponential(alpha, beta, n)
        invader.mat[1,2] <- exponential(alpha, beta, n)
        # new matrices
        resident.stage <- resident.mat %*% resident.stage
        invader.stage <- invader.mat %*% invader.stage
        pop.invader[i] <- invader.stage[2]
    }

    gen.start <- 20
    gen.end <- maxgen2
    generation <- seq(from=gen.start, to=gen.end)

    # linear regression
    invasion.model <- lm(log(pop.invader[gen.start:gen.end]) ~ generation[gen.start:gen.end])
    # elasticity value = regression slope
    elasticity <- invasion.model$coeff[2]
    
    return(elasticity)
}


## #### MAIN PROGRAM ####

Min.A = 0.1; Max.A = 0.6
Coeff = 0.995
# calculate optimum
Optimum = uniroot(pop.dynamics, interval=c(Min.A, Max.A), Coeff)
Best.Alpha = Optimum$root
print('Optimum reproductive effort')
print(Best.Alpha)

# plot elasticity vs alpha
N.Int = 30
Low = Min.A
High = Max.A
Alpha = matrix(seq(from=Low, to=High, length=N.Int), N.Int, 1)
Elasticity = apply(Alpha, 1, pop.dynamics, Coeff)
plot(Alpha, Elasticity, type='l')
lines(c(0.01, 0.9), c(0, 0))

Coeffs = Alpha / Best.Alpha
Invasion.Exp = matrix(0, N.Int, 1) # allocate matrix
# calculate invasion coefficient
for(i in 1:N.Int) {
    Invasion.Exp[i] = pop.dynamics(Best.Alpha, Coeffs[i])
}

plot(Alpha, Invasion.Exp, type='l')
points(Best.Alpha, 0, cex=2)
