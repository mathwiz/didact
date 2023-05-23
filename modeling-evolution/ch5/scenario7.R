# Rock-Paper-Scissors quantitative genetic model

# General Assumptions
# 1. The population consists of three behavioral clones: Rock, Paper, Scissors
# 2. The payoffs are symmetrical.
# 3. Fitness is equal to some initial quantity plus payoff.

# Payoff Matrix
#                       Opponent
# Self       Rock        Paper       Scissors
# Rock       -epsilon    -1           1 
# Paper       1          -epsilon    -1
# Scissors   -1          1           -epsilon


rm(list=ls())
library(MASS)

# Code: Rock=1, Scissors=2, Paper=3
Rock = 1
Scissors = 2
Paper = 3

# Payoff matrix
Epsilon = 0.5    
Payoff.Mat = matrix(c(-Epsilon,-1,1,1,-Epsilon,-1,-1,1,-Epsilon), 3, 3)

fitness = function(morph, trait.a, n.pop, payoff.mat) {
    opponent <-sample(morph)
    fit <- matrix(0, n.pop, 1)
    for(i.receiver in 1:3) {
        for(i.opponent in 1:3) {
            fit[morph==i.receiver & opponent==i.opponent] <- 3 + payoff.mat[i.receiver, i.opponent]
        }
    }
    sum.fit = sum(fit)
    mu.x1 <- sum(fit * trait.a[,1]) / sum.fit
    mu.x2 <- sum(fit * trait.a[,2]) / sum.fit
    mu.x3 <- sum(fit * trait.a[,3]) / sum.fit
    return(c(mu.x1, mu.x2, mu.x3))
}

#### MAIN PROGRAM ####
set.seed(100)

N.Pop = 500
Maxgen = 1000
Output = matrix(0, Maxgen, 4)
NX = 3  # number of traits
H2 = matrix(c(0.5, 0, 0, 0, 0.5, 0, 0, 0, 0.5), 3, 3)
Mu = c(0,0,0)  # genetic means
Var = c(1,1,1)  # variances, set and remain 1
Mean.A = c(Mu[1], Mu[2], Mu[3])  # initial additive genetic means
Mean.E = c(Mu[1], Mu[2], Mu[3])  # initial environmental means; is this ever mutated?
# phenotypic covariance matrix; note initial values are set to 1 (arbitrary)
CovP = matrix(1, NX, NX)
CovA = matrix(0, NX, NX)
CovE = matrix(0, NX, NX)
diag(CovP) = c(Var[1], Var[2], Var[3])

for(i in 1:NX) {
    CovA[i, i] = CovP[i, i] * H2[i, i]  # Vp * h2
    CovE[i, i] = CovP[i, i] - CovA[i, i]
    if(CovE[i, i] < 0) stop(print(c("CovE cannot be", i, CovE[i, i])))
}
# calculate phenotypic, environmental, and genetic covariances
for(i in 1:(NX-1)) {
    jj = i + 1
    for(j in jj:NX) {
        CovP[i, j] = H2[j, i] * sqrt(CovP[i, i] * CovP[j, j])
        CovP[j, i] = CovP[i, j]
        CovA[j, i] = CovA[i, j]
        CovE[i, j] = CovP[i, j] - CovA[i, j]
        CovE[j, i] = CovE[i, j]
    }    
}    

Morph = matrix(Paper, N.Pop, 1)  # initialize to Paper

for(i in 1:Maxgen) {
    Output[i, 1] = i
    # randomly generate additive and environmental values
    Trait.E = mvrnorm(N.Pop, mu=Mean.E, Sigma=CovE)
    Trait.A = mvrnorm(N.Pop, mu=Mean.A, Sigma=CovA)
    Trait.P = Trait.A + Trait.E
    # determine morph from trait with max value
    Morph[Trait.P[,1] > Trait.P[,2] & Trait.P[,1] > Trait.P[,3]] = Rock
    Morph[Trait.P[,1] > Trait.P[,2] & Trait.P[,1] < Trait.P[,3]] = Paper
    Morph[Trait.P[,1] < Trait.P[,2] & Trait.P[,1] > Trait.P[,3]] = Scissors

    Morph[Trait.P[,2] > Trait.P[,1] & Trait.P[,2] > Trait.P[,3]] = Scissors
    Morph[Trait.P[,2] > Trait.P[,1] & Trait.P[,2] < Trait.P[,3]] = Paper
    Morph[Trait.P[,2] < Trait.P[,1] & Trait.P[,2] > Trait.P[,3]] = Rock
    
    Morph[Trait.P[,3] > Trait.P[,1] & Trait.P[,3] > Trait.P[,2]] = Paper
    Morph[Trait.P[,3] > Trait.P[,1] & Trait.P[,3] < Trait.P[,2]] = Scissors
    Morph[Trait.P[,3] < Trait.P[,1] & Trait.P[,3] > Trait.P[,2]] = Rock

    N.Rock = length(Morph[Morph==Rock])
    N.Scissors = length(Morph[Morph==Scissors])
    N.Paper = length(Morph[Morph==Paper])

    Output[i, 2] = N.Rock / N.Pop
    Output[i, 3] = N.Scissors / N.Pop
    Output[i, 4] = N.Paper / N.Pop

    # apply selection
    Mean.A = fitness(Morph, Trait.A, N.Pop, Payoff.Mat)
}    


par(mfrow=c(2,2))

Offset1 = 0.5
Offset2 = Offset1 * 2
Startgen = 400

plot(Output[,1], Output[,2], type='l', xlab='Generation', ylab='Prop (+ offset)', ylim=c(0, 2.0))
lines(Output[,1], Output[,3] + Offset1, lty=2)
lines(Output[,1], Output[,4] + Offset2, lty=3)
# add lines for expected values
lines(Output[,1], rep(1/3, Maxgen))
lines(Output[,1], rep(1/3 + Offset1, Maxgen))
lines(Output[,1], rep(1/3 + Offset2, Maxgen))

# phase plots of pairwise morphs
plot(Output[Startgen:Maxgen,2], Output[Startgen:Maxgen,3], type='l', xlab='Rock', ylab='Scissors')
plot(Output[Startgen:Maxgen,2], Output[Startgen:Maxgen,4], type='l', xlab='Rock', ylab='Paper')
plot(Output[Startgen:Maxgen,3], Output[Startgen:Maxgen,4], type='l', xlab='Scissors', ylab='Paper')

print(c('Mean Proportions (R, S, P) from Generation', Startgen, 'to', Maxgen))
print(c(mean(Output[Startgen:Maxgen, 2]), mean(Output[Startgen:Maxgen, 3]), mean(Output[Startgen:Maxgen, 4])))
