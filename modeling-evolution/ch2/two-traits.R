rm(list=ls())

fitness = function(x, a.xy, a0, b.xy, c.xy, b.yx, c.yx) {
    vigilance <- x[1]
    foraging <- x[2]
    s0 <- a.xy * vigilance * foraging - a0
    s.xy <- -b.xy * vigilance + c.xy * vigilance^2
    s.yx <- -b.yx * foraging + c.yx * foraging^2
    w <- s0 - s.xy - s.yx
    return(w)
}    


#### Main program ####
A.0 = B.xy = B.yx = 0.8
A.xy = C.xy = C.yx = 0.4

Points = 100
X = seq(from=0.0, to=3.0, length=Points)
S.xy = -B.xy*X + C.xy*X^2

# contour data
N = 20
Vigilance = seq(from=1.0, to=3.0, length=N)
Foraging = seq(from=1.0, to=3.0, length=N)
D = expand.grid(Vigilance, Foraging)
# fitness values for all combos
W.Temp = apply(D, 1, fitness, A.xy, A.0, B.xy, C.xy, B.yx, C.yx)
# convert into matrix
W = matrix(W.Temp, N, N, byrow=T)

# find maximum using calculus
derivative = function(x) {
    t1 <- abs(0.4*x[2] + 0.8 - 2.0*0.4*x[1])
    t2 <- abs(0.4*x[1] + 0.8 - 2.0*0.4*x[2])
    return(t1 + t2)
}
Max.Symb = nlm(derivative, p=c(1,1))$estimate

# find maximum numerically
fitness2 = function(x) {
    t1 <- A.xy * x[1] * x[2]
    t3 <- B.xy * x[1]
    t4 <- C.xy * x[1]^2
    t5 <- B.yx * x[2]
    t6 <- C.yx * x[2]^2
    w <- t1 - A.0 + t3 - t4 + t5 - t6
    return(-w)
}

Traits = nlm(fitness2, p=c(0.5,0.5))$estimate
Max.Num = -fitness2(Traits)

print('two-traits done.')

output = function() {
    par(mfrow=c(2,2))
    plot(X, S.xy, type='l', xlab='Vigilance or Foraging Rate', ylab='Effect on Survival', las=1, lwd=3)
    contour(Vigilance, Foraging, W, xlab='Vigilance', ylab='Foraging', las=1, lwd=3, labcex=1)
    persp(Vigilance, Foraging, W, xlab='Vigilance', ylab='Foraging', zlab='Fitness', theta=50, phi=25, lwd=2)
    print('Maximum using calculus')
    print(Max.Symb)
    print('Maximum numerically')
    print(c(Traits, Max.Num))
}    
