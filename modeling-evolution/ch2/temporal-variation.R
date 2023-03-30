rm(list=ls())
set.seed(10)

fitness = function(x) {
    pbs <- c(0.1, 0.30, 0.40, 0.2)
    bs  <- c(0.1, 0.12, 0.14, 0.2)
    w.ind <- (Af + Bf*x) * (As - bs*x)
    return(-sum(pbs * log(w.ind)))
}    

#### Main program ####
Af = 2.0; Bf = 2.0; As = 0.6

Points = 100
N = 1000
Body.Size = matrix(seq(from=0, to=2.99, length=Points))
Fitness.Log = apply(Body.Size, 1, fitness)
Fitness = exp(-Fitness.Log)

## for(i in 1:Points) {
##     Surv = As - Bs*Body.Size[i]
##     # ensure valid range
##     Surv[Surv < 0] = 0
##     Surv[Surv > 1] = 1
##     Fitness[i, 1] = mean((Af + Bf*Body.Size[i]) * Surv)
##     Fitness[i, 2] = (Af + Bf*Body.Size[i]) * (Amean - Bmean*Body.Size[i])
## }    

## # find maximum numerically
## fitness = function(x, as, bs) {
##     af <- 2.0; bf <- 2.0
##     Surv = as - bs*x
##     # ensure valid range
##     Surv[Surv < 0] = 0
##     Surv[Surv > 1] = 1
##     W = mean((af + bf*x) * Surv)
##     return(-W)
## }

## # use more data for numeric integration
## N = 10000
## N.Reps = 100
## Replicates = matrix(0, N.Reps)
## for(i in 1:N.Reps) {
##     Bs = runif(N, min=Bmin, max=Bmax)
##     As = runif(N, min=Amin, max=Amax)
##     Replicates[i] = nlm(fitness, p=Replicates[i], As, Bs)$estimate
## }    

print('temporal-variation done.')

output = function() {
    par(mfcol=c(1,1))
    plot(Body.Size, Fitness, type='l', xlab='Body Size', ylab='Fitness, W', las=1, lwd=4)
    ## lines(Body.Size, Fitness[,2], lwd=4)
    ## print("Fitness by numeric integration (Mean, SD)")
    ## print(c(mean(Replicates), sd(Replicates)))
}    
