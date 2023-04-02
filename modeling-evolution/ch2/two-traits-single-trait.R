rm(list=ls())

fitness = function(x1) {
    exp.fecundity1 <- F.Max*(1 - exp(-A*x1))
    x2 <- R/N - x1
    x2 <- max(x2, 0) # will replace negatives with 0
    exp.fecunity2 <- F.Max*(1 - exp(-A*x2))
    w <- N*(S1*exp.fecundity1 + S2*exp.fecunity2)
    if(N*x1 > R) { w <- 0 }
    return(w)
}    


#### Main program ####
S1 = 0.005; S2 = 0.002
F.Max = 2.0; A = 1
N = 100
R = 400

X = matrix(seq(from=0.0, to=4.0, length=N))
W = apply(X, 1, fitness)


## # find maximum using calculus
## derivative = function(x) {
##     t1 <- abs(0.4*x[2] + 0.8 - 2.0*0.4*x[1])
##     t2 <- abs(0.4*x[1] + 0.8 - 2.0*0.4*x[2])
##     return(t1 + t2)
## }
## Max.Symb = nlm(derivative, p=c(1,1))$estimate

## # find maximum numerically
## fitness2 = function(x) {
##     t1 <- A.xy * x[1] * x[2]
##     t3 <- B.xy * x[1]
##     t4 <- C.xy * x[1]^2
##     t5 <- B.yx * x[2]
##     t6 <- C.yx * x[2]^2
##     w <- t1 - A.0 + t3 - t4 + t5 - t6
##     return(-w)
## }

## Traits = nlm(fitness2, p=c(0.5,0.5))$estimate
## Max.Num = -fitness2(Traits)

print('two-traits-single-trait done.')

output = function() {
    par(mfrow=c(1,1))
    plot(X, W, type='l', xlab='Propagule size', ylab='Fitness, R0', las=1, lwd=3)
    ## contour(Vigilance, Foraging, W, xlab='Vigilance', ylab='Foraging', las=1, lwd=3, labcex=1)
    ## persp(Vigilance, Foraging, W, xlab='Vigilance', ylab='Foraging', zlab='Fitness', theta=50, phi=25, lwd=2)
    ## print('Maximum using calculus')
    ## print(Max.Symb)
    ## print('Maximum numerically')
    ## print(c(Traits, Max.Num))
}    
