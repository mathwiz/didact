rm(list=ls())

# A quantitative genetic analysis of the Ricker model

# General assumptions
# 1. The organism is semelparous
# 2. Recruitment is governed by a density-dependence function that allows for cyclical or chaotic dynamics.
# 3. The parameters of the recruitment function are related such that the density-independent component is negatively related to the density-dependent component.
# 4. These parameters are inherited.


selection = function(xs, n) {
    beta <- xs[,1] * 0.001
    x.fitness <- xs[,1] * exp(-beta*n)
    sum.fit <- sum(x.fitness)
    mu <- sum(x.fitness * xs[,2]) / sum.fit
    return(c(mu, round(sum.fit)))
}


#### MAIN PROGRAM ####
set.seed(100)

N = 100
Maxgen = 10^4
Output = matrix(0, Maxgen, 3)
H2 = 0.5                # heritability
Vp = 0.1                # phenotypic variance
Va = Vp * H2            # additive genetic variance
Ve = Vp - Va            # environmental variance
Mu = 10                 # Trait mean genetic value
SDa = sqrt(Va)          # by def of sd
SDe = sqrt(Ve)          # by def of sd

for(i in 1:Maxgen) {
    G.X = rnorm(N, mean=Mu, sd=SDa)
    E.X = rnorm(N, mean=0, sd=SDe)
    # phenotypic is sum of genetic and environmental
    P.X = G.X + E.X 
    # combine phenotypic and genetic
    X = cbind(P.X, G.X)
    Output[i, 1] = i        
    Output[i, 2] = mean(P.X)
    Output[i, 3] = N
    # calculate new mean genetic value by applying fitness
    B = selection(X, N)
    Mu = B[1]
    N = B[2]
}    

# graphing parameters
par(mfrow=c(2,2))
Startgen = 10
plot(Output[Startgen:Maxgen,1], Output[Startgen:Maxgen,2], type='l', xlab='Generation', ylab='Trait Value')
plot(Output[Startgen:Maxgen,1], Output[Startgen:Maxgen,3], type='l', xlab='Generation', ylab='Population Size')
print('Mean Trait Value and Mean Pop Size ')
print(c(mean(Output[2000:Maxgen, 2]), mean(Output[2000:Maxgen, 3])))



