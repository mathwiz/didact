rm(list=ls())

bh = function(n, c1, c2) {
    c1 / (1 + c2 * n)
}

ricker = function(n, alpha, beta) {
    alpha * exp(-beta*n)
}    

#### MAIN PROGRAM ####
par(mfrow=c(3,2))

# Beverton-Holt
C1 = 100; C2 = 2*10^-3
Maxgen = 20
N.T = matrix(0, Maxgen)
N.T[1] = 1
Generation = seq(from=1, to=Maxgen)

for(i in 2:Maxgen) {
    N.T[i] = N.T[i-1] * bh(N.T[i-1], C1, C2)
}    
Max.N = 5000
N.T2 = matrix(seq(from=1, to=Max.N))
N.T.Plus1 = N.T2 * apply(N.T2, 1, bh, C1, C2)

# Beverton-Holt
plot(Generation, N.T, type='l', xlab='Generation, t', ylab='N(t)')
plot(N.T2, N.T.Plus1, type='l', xlab='N(T)', ylab='N(t+1)')

# Ricker
Alpha = c(6, 60); Beta = 0.0005
Maxgen.R = 40
N.T.R = matrix(0, Maxgen.R)
N.T.R[1] = 1
Generation.R = seq(from=1, to=Maxgen.R)
Max.N.R = 10^4
N.T2.R = matrix(seq(from=1, to=Max.N.R))

for(j in 1:2) {
    for(i in 2:Maxgen.R) {
        N.T.R[i] = N.T.R[i-1] * ricker(N.T.R[i-1], Alpha[j], Beta)
    }    
    plot(Generation.R, N.T.R, type='l', xlab='Generation, t', ylab='N(t)')
    N.T.Plus1.R = N.T2.R * apply(N.T2.R, 1, ricker, Alpha[j], Beta)
    plot(N.T2.R, N.T.Plus1.R, type='l', xlab='N(T)', ylab='N(t+1)')
    lines(N.T2.R, N.T2.R)
}


output = function() {
    print('All results in figure')
}    
