rm(list=ls())

# Stabilizing selection using an Individual Variance Components model

# General assumptions
# 1. Organism is semelparous
# 2. Fecundity, F, increases with body size, x
# 3. Survival, S, decreases with body size, x
# 4. Fitness, W, is a function of F and S
# 5. Body size is an inherited quantitative trait

selection = function(xs) {
    as <- 1; bs <- 0.5; af <- 0; bf <- 4
    survival <- as - bs*xs[,1]
    survival[survival < 0] <- 0
    fecundity <- af + bf*xs[,1]
    fecundity[fecundity < 0] <- 0
    x.fitness <- survival * fecundity
    mu <- sum(x.fitness*xs[,2]) / sum(x.fitness)
    return(mu)
}

#### MAIN PROGRAM ####
set.seed(100)

N = 100
Maxgen = 2000
Output = matrix(0, Maxgen, 2)
H2 = 0.5         # heritability
Vp = (.1)^2      # phenotypic variance
Va = H2 * Vp     # since H2 = Va / Vp
Ve = Vp - Va     # environmental variance
Mu = 1.5         # initial trait mean value
SDa = sqrt(Va)   # by def of sd
SDe = sqrt(Ve)   # by def of sd

for(i in 1:Maxgen) {
    GX = rnorm(N, mean=Mu, sd=SDa)
    EX = rnorm(N, mean=0, sd=SDe)
    FX =  GX + EX
    X = cbind(FX, GX)
    Output[i, 1] = i
    Output[i, 2] = mean(FX)
    Mu = selection(X)
}    

# graphing parameters
par(mfrow=c(1,1))
plot(Output[,1], Output[,2], type='l', xlim=c(0, Maxgen), xlab='Generation', ylab='Trait')
print('Mean phenotype')
print(mean(Output[500:Maxgen, 2]))
print('SD of mean')
print(sd(Output[500:Maxgen, 2]))
