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


# find maximum using calculus
derivative.calc = function(x) {
    t1 <- S1 * exp(-A*x)
    t2 <- S2 * exp(-A*(R/N - x))
    return(t1 - t2)
}
Max.Symb = uniroot(derivative.calc, interval=c(0,3))$root
Size.Second = (R/N) - Max.Symb

# alternative method
derivative.symb = function(w) {
    y <- deriv(~ N * (S1*F.Max*(1-exp(-A*x)) + S2*F.Max*(1-exp(-A*(R/N -x)))), "x")
    x <- w
    z <- eval(y)
    d <- attr(z, "gradient")
    return(d)
}    
Max.Symb2 = uniroot(derivative.symb, interval=c(0,3))$root
Size.Second2 = (R/N) - Max.Symb2

# find maximum numerically
Max.Num = optimize(f=fitness, interval=c(1,8), maximum=T)$maximum
Size.Second.Num = (R/N) - Max.Num

print('two-traits-single-trait done.')

output = function() {
    par(mfrow=c(1,1))
    plot(X, W, type='l', xlab='Propagule size', ylab='Fitness, R0', las=1, lwd=3)
    print('Maximum using calculus')
    print(c(Max.Symb, Size.Second))
    print(c(Max.Symb2, Size.Second2))
    print('Maximum numerically')
    print(c(Max.Num, Size.Second.Num))
}    
