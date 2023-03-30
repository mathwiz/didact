rm(list=ls())
set.seed(10)

fitness = function(x) {
    w.ind <- (Af + Bf*x) * (As -Bs*x)
    return(-sum(P.Bs * log(w.ind)))
}    

derivative = function(w) {
    # iterate over values of bs and sum balues of derivatives
    d <- 0
    for(i in 1:length(Bs)) {
        bsi <- Bs[i]
        pbsi <- P.Bs[i]
        y <- deriv(~ pbsi * log((Af + Bf*x) * (As - bsi*x)), 'x')
        x <- w # needed for symbolic deriv variable 'x'
        z <- eval(y)
        d <- d + attr(z, 'gradient')
    }    

    return(d)
}    

#### Main program ####
Af = 2.0; Bf = 2.0; As = 0.6
P.Bs <- c(0.1, 0.30, 0.40, 0.2)
Bs  <- c(0.1, 0.12, 0.14, 0.2)

Points = 100
N = 1000
Body.Size = matrix(seq(from=0, to=2.99, length=Points))
Fitness.Log = apply(Body.Size, 1, fitness)
Fitness = exp(-Fitness.Log)

# find maximum using calculus
Max.Root.Symb = uniroot(derivative, interval=c(1,2))$root

# find maximum with numeric integration
Max.Root.Num1 = nlm(fitness, p=1)$estimate
Max.Root.Num2 = optimize(f=fitness, interval=c(1,2), maximum=FALSE)$minimum

print('temporal-variation done.')

output = function() {
    par(mfcol=c(1,1))
    plot(Body.Size, Fitness, type='l', xlab='Body Size', ylab='Fitness, W', las=1, lwd=4)
    print("Fitness by symbolic calculation")
    print(Max.Root.Symb)
    print("Fitness by numeric integration (nlm, optimize)")
    print(c(Max.Root.Num1, Max.Root.Num2))
}    
