rm(list=ls())

S1 = 0.035; S2 = 0.030; S3 = 0.025
F.Max = 2.0; A = 0.1
N = 100
R = 400

fitness = function(x) {
    # x[1], x[2] are propagule size in first, second clutch
    w1 <- w2 <- w3 <- 0
    # check if first clutch exceeds reserves
    if(N*x[1] > R) {
        w <- 0
    } else {
        w1 <- N*S1*F.Max*(1-exp(-A*x[1]))

        # check second clutch
        if(N*(x[1] + x[2]) > R) {
            w <- w1
        } else {
            w2 <- N*S2*F.Max*(1-exp(-A*x[2]))

            # calculate third clutch
            x3 <- (R - N*(x[1] + x[2])) / N
            w3 <- N*S3*F.Max*(1-exp(-A*x3))
            w <- w1 + w2 + w3
        }    
    }    
    
    return(-w)
}    


#### Main program ####

Size = 20
X = seq(from=1.0, to=5.0, length=Size)
D = expand.grid(X, X)
W = apply(D, 1, fitness)
W = matrix(W, Size, Size)

## # find maximum using calculus
## derivative.calc = function(x) {
##     t1 <- S1 * exp(-A*x)
##     t2 <- S2 * exp(-A*(R/N - x))
##     return(t1 - t2)
## }
## Max.Symb = uniroot(derivative.calc, interval=c(0,3))$root
## Size.Second = (R/N) - Max.Symb

## # alternative method
## derivative.symb = function(w) {
##     y <- deriv(~ N * (S1*F.Max*(1-exp(-A*x)) + S2*F.Max*(1-exp(-A*(R/N -x)))), "x")
##     x <- w
##     z <- eval(y)
##     d <- attr(z, "gradient")
##     return(d)
## }    
## Max.Symb2 = uniroot(derivative.symb, interval=c(0,3))$root
## Size.Second2 = (R/N) - Max.Symb2

## # find maximum numerically
## Max.Num = optimize(f=fitness, interval=c(1,8), maximum=T)$maximum
## Size.Second.Num = (R/N) - Max.Num

print('three-traits done.')

output = function() {
    par(mfrow=c(1,1))
    #contour(X, X, -W, xlab='Propagule size 1st clutch', ylab='Popagule size 2nd clutch')
    persp(X, X, W, xlab='Propagule size 1st clutch', ylab='Popagule size 2nd clutch', zlab='Fitness', theta=25, phi=25, lwd=1)
    ## print('Maximum using calculus')
    ## print(c(Max.Symb, Size.Second))
    ## print(c(Max.Symb2, Size.Second2))
    ## print('Maximum numerically')
    ## print(c(Max.Num, Size.Second.Num))
}    
