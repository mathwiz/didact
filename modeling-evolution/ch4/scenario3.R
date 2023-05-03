rm(list=ls())

# Directional Selection using an Individual Variance Components model

# General assumptions
# 1. The trait under selection has two phenotypic expressions but is determined by an underlying normally distributed liability.
# 2. Only one morph is allowed to contribute to the next generation.

# Two morphs are coded 0 (long wing), 1 (short wing)

selection = function(male , female) {
    mu.m <- sum(male[,3] * male[,2]) / sum(male[,3])
    mu.f <- sum(female[,3] * female[,2]) / sum(female[,3])
    mu <- (mu.m + mu.f) / 2
    return(mu)
}

#### MAIN PROGRAM ####
set.seed(100)

N = 100
Prop.LW = 0.85
Z.LW = -qnorm(Prop.LW)
Maxgen = 10
Output = matrix(0, Maxgen, 4)
H2 = 0.5         # heritability
Vp = 1.0         # phenotypic variance
Va = H2 * Vp     # since H2 = Va / Vp
Ve = Vp - Va     # environmental variance
Mu = 0.0         # initial trait mean value
SDa = sqrt(Va)   # by def of sd
SDe = sqrt(Ve)   # by def of sd

for(i in 1:Maxgen) {
    G.M = rnorm(N, mean=Mu, sd=SDa)
    G.F = rnorm(N, mean=Mu, sd=SDa)
    E.M = rnorm(N, mean=0, sd=SDe)
    E.F = rnorm(N, mean=0, sd=SDe)
    P.M = G.M + E.M
    P.F = G.F + E.F
    # calculate wing morphs by comparing liability to threshold
    Morph.M = matrix(1, N)
    Morph.M[P.M > Z.LW] = 0
    Morph.F = matrix(1, N)
    Morph.F[P.F > Z.LW] = 0
    Male = cbind(P.M, G.M, Morph.M)
    Female = cbind(P.F, G.F, Morph.F)
    Output[i, 1] = i
    Output[i, 2] = mean(P.M + P.F) / 2
    Output[i, 3] = sum(Morph.M) / N
    Output[i, 4] = sum(Morph.F) / N
    # calculate new mean genetic value by applying fitness criterion
    Mu = selection(Male, Female)
}    

# graphing parameters
par(mfrow=c(2,2))
plot(Output[,1], Output[,3], pch='M', xlim=c(0, Maxgen), xlab='Generation', ylab='Proportion Short Wings')
lines(Output[,1], Output[,3]) # Males
points(Output[,1], Output[,4], pch='F') # Females
lines(Output[,1], Output[,4]) # Females
plot(Output[,1], Output[,2], xlim=c(0, Maxgen), xlab='Genefration', ylab='Mean Liability')
lines(Output[,1], Output[,2])
