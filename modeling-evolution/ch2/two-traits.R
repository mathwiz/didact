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


print('two-traits done.')

output = function() {
    par(mfrow=c(2,2))
    plot(X, S.xy, type='l', xlab='Vigilance or Foraging Rate', ylab='Effect on Survival', las=1, lwd=3)
    contour(Vigilance, Foraging, W, xlab='Vigilance', ylab='Foraging', las=1, lwd=3, labcex=1)
    persp(Vigilance, Foraging, W, xlab='Vigilance', ylab='Foraging', zlab='Fitness', theta=50, phi=25, lwd=2)
}    
