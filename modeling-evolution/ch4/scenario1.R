rm(list=ls())

# Stabilizing selection using an Population Variance Components model

# General assumptions
# 1. Both traits are quantitative with non-zero probabilities
# 2. Selection is stabilizing.
# 3. Generations do not overlap.

H2 = c(0.2, 0.4) # heritability
Vp = c(1.0, 2.0) # phenotypic variance
Va = H2 * Vp     # since H2 = Va / Vp
Rp = 0.4         # phenotypic correlation
Ra = 0.15        # genetic correlation

# check that Re is possible
Re = (Rp - Ra*sqrt(H2[1]*H2[2])) / sqrt((1 - H2[1]) * (1 - H2[2]))
if(abs(Re) > 1) {
    print('Problem with Re')
    stop
}    

Co.Vp = Rp * sqrt(Vp[1] * Vp[2])
Co.Va = Ra * sqrt(Va[1] * Vp[2])
G.Mat = matrix(c(Va[1], Co.Va, Co.Va, Va[2]), 2, 2)
P.Mat = matrix(c(Vp[1], Co.Vp, Co.Vp, Vp[2]), 2, 2)
W.Mat = matrix(c(2, 0, 0, 2), 2, 2)
Theta = c(2,2)
Maxgen = 100
Trait = matrix(0, Maxgen, 2)
Trait[1,] = 10 # initial row

for(i in 2:Maxgen) {
    Delta.Z = G.Mat %*% solve(W.Mat + P.Mat) %*% (Trait[i-1,] - Theta)
    Trait[i,] = Trait[i-1,] - Delta.Z
}    

# graphing parameters
par(mfrow=c(1,2))
Min.Y = Min.X = min(Trait)
Max.Y = Max.X = max(Trait)
Gen = seq(from=1, to=Maxgen)
plot(Gen, Trait[,1], type='l', xlim=c(0, Maxgen), ylim=c(Min.Y, Max.Y), xlab='Generation', ylab='Trait')
lines(seq(from=1, to=Maxgen), Trait[,2], lty=2)
# plot Trait 2 vs Trait 1
plot(Trait[,1], Trait[,2])
lines(Trait[,1], Trait[,2])

print('Rp Ra Re')
print(c(Rp, Ra, Re))
print('Eigenvalue of W')
print(eigen(W.Mat)$value)
