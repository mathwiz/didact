rm(list=ls())
library(MASS)

# Evolution of two traits using an Individual Locus model

# General assumptions
# 1. Generations ore non-overlapping
# 2. There is a negative correlation (both genetic and phenotypic) between the two traits X and Y.
# 3. Fitness is a threshold value such that individuals above some value contribute equally to the next generation while below threshold have zero fitness.


assign.loci = function(g.loci, n.pop, p) {
    total.loci <- 2*n.pop*g.loci
    alleles <- runif(total.loci)
    alleles[alleles < p] <- 0
    alleles[alleles > p] <- 1
    return(alleles)
}    

selection = function(phenotype, genotype, t0) {
    genotype[phenotype > t0, ]
}

gamete = function(xs, g.loci) {
    sample(x=xs, size=g.loci, replace=F)
}    

mutation = function(xs, p.mut, g.loci, n.ind) {
    t.loci <- 2*n.ind*g.loci
    lambda <- p.mut*t.loci
    n.mutations <- rpois(1, lambda)
    row <- ceiling(runif(n.mutations, min=0, max=t.loci))
    temp <- matrix(xs)
    temp[row] <- abs(temp[row] - 1)
    return(matrix(temp, n.ind, 2*g.loci))
}

#### MAIN PROGRAM ####
set.seed(100)

N = 1000                # population size at each generation
Pmut = 0.0001           # mutation probability
X.Loci = Y.Loci = 30    # loci per gamete unique to X
C.Loci = 25             # loci per gamete common to X and Y
H2.X = 0.4              # heritability of X
H2.Y = 0.5              # heritability of Y
S = -1                  # sign of genetic correlation
Rp = -0.7               # phenotypic correlation
Pxy = 0.68              # proportion at x or y loci
Pc = 0.47               # proportion of c loci
Trait.X = 2*(Pxy*X.Loci + Pc*C.Loci)
Trait.Y = 2*(Pxy*Y.Loci + S*Pc*C.Loci)

Var.GX = 2*(X.Loci*Pxy*(1-Pxy) + C.Loci*Pc*(1-Pc)) # additive genotypic variance
Var.GY = Var.GX
Cov.GXY = 2*C.Loci*Pc*(1-Pc)
Rg = S*Cov.GXY / sqrt(Var.GX*Var.GY)

print('Rg Trait.X Trait.Y')
print(c(Rg, Trait.X, Trait.Y))

Re = (Rp - Rg*sqrt(H2.X*H2.Y)) / sqrt((1-H2.X)*(1-H2.Y))
if(abs(Re) > 1) stop (c('Re not possible'))

# environmental variances and standard deviations
Var.EX = (1-H2.X)*Var.GX / H2.X
SD.EX = sqrt(Var.EX)
Var.EY = (1-H2.Y)*Var.GY / H2.Y
SD.EY = sqrt(Var.EY)
Cov.E = Re*SD.EX*SD.EY
E.Mat = matrix(c(Var.EX, Cov.E, Cov.E, Var.EY), 2, 2)  # covariance matrix

# numbers of loci in each category
N.Alleles.X = assign.loci(X.Loci, N, Pxy)
N.Alleles.Y = assign.loci(Y.Loci, N, Pxy)
N.Alleles.C = assign.loci(C.Loci, N, Pc)
G.Mat.X = matrix(N.Alleles.X, N, 2*X.Loci)
G.Mat.Y = matrix(N.Alleles.Y, N, 2*Y.Loci)
G.Mat.C = matrix(N.Alleles.C, N, 2*C.Loci)

# iterate over generations
Maxgen = 40
Output = matrix(0, Maxgen, 9)
T0 = matrix(Trait.X, Maxgen, 1)
T0[1:5] = -100 # set T0 so that first 5 gens don't have selection

for(i in 1:Maxgen) {
    G.X = rowSums(G.Mat.X) + rowSums(G.Mat.C)
    Var.GX = var(G.X)
    G.Y = rowSums(G.Mat.Y) + S*rowSums(G.Mat.C)
    Var.GY = var(G.Y)
    # create phenotypic values
    Env = mvrnorm(n=N, mu=c(0,0), Sigma=E.Mat)
    P.X = G.X + Env[,1]
    P.Y = G.Y + Env[,2]
    Var.PX = var(P.X)
    Var.PY = var(P.Y)
    H2.X = Var.GX / Var.PX
    H2.Y = Var.GY / Var.PY
    Rg = cor(G.X, G.Y)
    Rp = cor(P.X, P.Y)
    
    Output[i, 1:9] = c(i, Var.GX, Var.GY, mean(P.X), mean(P.Y), H2.X, H2.Y, Rg, Rp)

    # apply selection; note that selection here is only a function of X
    Parent.X = selection(P.X, G.Mat.X, T0[i])
    Parent.Y = selection(P.X, G.Mat.Y, T0[i])
    Parent.C = selection(P.X, G.Mat.C, T0[i])
    # form gamete pool
    Gamete.X = apply(Parent.X, 1, gamete, X.Loci)
    Gamete.X = t(Gamete.X)    
    Gamete.Y = apply(Parent.Y, 1, gamete, Y.Loci)
    Gamete.Y = t(Gamete.Y)    
    Gamete.C = apply(Parent.C, 1, gamete, C.Loci)
    Gamete.C = t(Gamete.C)    
    N.Parents = seq(1, nrow(Parent.X)) # avoid author's use of n for this
    N1 = N + 1
    N2 = 2*N
    
    # form next generation
    G.Index = sample(x=N.Parents, size=N2, replace=T)
    S.Gamete.X = Gamete.X[G.Index,]
    S.Gamete.Y = Gamete.Y[G.Index,]
    S.Gamete.C = Gamete.C[G.Index,]
    G.Mat.X = cbind(S.Gamete.X[1:N,], S.Gamete.X[N1:N2,])
    G.Mat.Y = cbind(S.Gamete.Y[1:N,], S.Gamete.Y[N1:N2,])
    G.Mat.C = cbind(S.Gamete.C[1:N,], S.Gamete.C[N1:N2,])
    
    # generate mutant loci
    G.Mat.X = mutation(G.Mat.X, Pmut, X.Loci, N)
    G.Mat.Y = mutation(G.Mat.Y, Pmut, Y.Loci, N)
    G.Mat.C = mutation(G.Mat.C, Pmut, C.Loci, N)
}    

# graphing parameters
par(mfrow=c(2,2))

ymin = min(Output[,4:5]);
ymax = max(Output[,4:5]);
plot(Output[,1], Output[,4], type='l', ylim=c(ymin, ymax), xlab='Generation', ylab='Phenotypes')
lines(Output[,1], Output[,5], lty=2)

ymin = min(Output[,2:3]);
ymax = max(Output[,2:3]);
plot(Output[,1], Output[,2], type='l', ylim=c(ymin, ymax), xlab='Generation', ylab='Genetic variances')
lines(Output[,1], Output[,3], lty=2)

ymin = min(Output[,6:7]);
ymax = max(Output[,6:7]);
plot(Output[,1], Output[,6], type='l', ylim=c(ymin, ymax), xlab='Generation', ylab='Heritabilities')
lines(Output[,1], Output[,7], lty=2)

ymin = min(Output[,8:9]);
ymax = max(Output[,8:9]);
plot(Output[,1], Output[,8], type='l', ylim=c(ymin, ymax), xlab='Generation', ylab='Correlations')
lines(Output[,1], Output[,9], lty=2)
