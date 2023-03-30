rm(list=ls())
set.seed(10)

#### Main program ####
Af = 2; Bf = 2
Amin = 0.3; Amax = 1.0
Bmin = 0.0; Bmax = 0.2
Amean = mean(c(Amin, Amax))
Bmean = mean(c(Bmin, Bmax))

Points = 100
N = 1000
Bs = runif(N, min=Bmin, max=Bmax)
As = runif(N, min=Amin, max=Amax)
Body.Size = seq(from=0, to=6, length=Points)
Fitness = matrix(0, Points, 2)

for(i in 1:Points) {
    Surv = As - Bs*Body.Size[i]
    # ensure valid range
    Surv[Surv < 0] = 0
    Surv[Surv > 1] = 1
    Fitness[i, 1] = mean((Af + Bf*Body.Size[i]) * Surv)
    Fitness[i, 2] = (Af + Bf*Body.Size[i]) * (Amean - Bmean*Body.Size[i])
}    

# find maximum numerically
fitness = function(x, as, bs) {
    af <- 2.0; bf <- 2.0
    Surv = as - bs*x
    # ensure valid range
    Surv[Surv < 0] = 0
    Surv[Surv > 1] = 1
    W = mean((af + bf*x) * Surv)
    return(-W)
}

# use more data for numeric integration
N = 10000
N.Reps = 100
Replicates = matrix(0, N.Reps)
for(i in 1:N.Reps) {
    Bs = runif(N, min=Bmin, max=Bmax)
    As = runif(N, min=Amin, max=Amax)
    Replicates[i] = nlm(fitness, p=Replicates[i], As, Bs)$estimate
}    

print('stochastic-variation done.')

output = function() {
    par(mfcol=c(1,1))
    plot(Body.Size, Fitness[,1], type='p', xlab='Body Size', ylab='Fitness, W', las=1, lwd=4)
    lines(Body.Size, Fitness[,2], lwd=4)
    print("Fitness by numeric integration (Mean, SD)")
    print(c(mean(Replicates), sd(Replicates)))
}    
