# Hawk-Dove mendelian model

# General assumptions
# 1. The population consists of two types of clones: Hawk, Dove.
# 2. Morph is determined by a single locus with two alleles: H, D.
# 3. A hawk interacting with a dove always wins.
# 4. A hawk interacting with a hawk earns a negative payoff.
# 5. A dove interacting with a dove divides the payoff.
# 6. Fitness is equal to some initial quantity plus the payoff.

# Genotypes: HH, HD are Hawk, DD is Dove

rm(list=ls())

fitness = function(morph, payoff.mat, n.hh, n.hd, npop) {
    opponent <- sample(morph)
    fit <- matrix(0, npop, 1)
    for(i.receiver in 1:2) {
        for(i.opponent in 1:2) {
            fit[morph==i.receiver & opponent==i.opponent] <- 3 + payoff.mat[i.receiver, i.opponent]
        }    
    }
    # now we know 1-n.hh are Hawk
    n1 <- n.hh + 1
    n2 <- n1 + n.hd - 1
    h.alleles <- 2*sum(fit[1:n.hh]) + sum(fit[n1:n2])
    n3 <- n2 + 1  # starting row of DD
    # now the numberof D alleles
    d.alleles <- sum(fit[n1:n2]) + 2*sum(fit[n3:npop])
    p.hawk <- h.alleles / (h.alleles + d.alleles)
    # return proportion of H alleles
    return(p.hawk)
}

#### MAIN PROGRAM ####
set.seed(100)
N.Pop = 2000
Morph = matrix(2, N.Pop, 1)
Max.Prop = 100  # number of divisions of proportion
# col 1=Prop.H, col 2=Delta Prop.H, col 3=P.Hawk, col 4=Delta P.Hawk
Output = matrix(0, Max.Prop, 4)
Payoff.Mat = matrix(c(-1, 0, 8, 4), 2, 2)
# give names to the 4 Payoff cells
A = Payoff.Mat[1,1]
B = Payoff.Mat[1,2]
C = Payoff.Mat[2,1]
D = Payoff.Mat[2,2]
# expected proportion of Hawk
P.Hawk = (B-D) / (B+C-A-D)
# proportion of H allele
Output[,1] = seq(from=0.01, to=0.95, length=Max.Prop)

for(i in 1:Max.Prop) {
    Prop.H = Output[i, 1]
    # find number of genontypes
    HH = round(Prop.H^2 * N.Pop)
    HD = round(2*Prop.H * (1-Prop.H) * N.Pop)
    Morph[1:N.Pop] = 2 # initialize to Dove
    N.Hawk = HH + HD
    Morph[1:N.Hawk] = 1 # make correct number of Hawks
    # calculate new proportion of H allele by applying fitness
    New.Prop.H = fitness(Morph, Payoff.Mat, HH, HD, N.Pop)
    Output[i, 2] = New.Prop.H - Prop.H
    Output[i, 3] = Prop.H^2 + 2*Prop.H*(1 - Prop.H)
    New.N.Hawk = New.Prop.H^2 + 2*New.Prop.H*(1 - New.Prop.H)
    Output[i, 4] = New.N.Hawk - Output[i, 3]
}


par(mfrow=c(1,1))
Y.Min = min(Output[,2], Output[,4])
Y.Max = max(Output[,2], Output[,4])
plot(Output[,1], Output[,2], type='l', lty=2, ylim=c(Y.Min, Y.Max), xlab='Initial Prop (P or Prop.H)', ylab='Change (in P or Prop.H)')
lines(Output[,3], Output[,4], type='l')  # change in Prop.H
lines(Output[,1], rep(0, Max.Prop)) # theoretical ESS horizontal line
points(P.Hawk, 0, pch='X', cex=2)
# splines for smoothing
lines(smooth.spline(Output[,1], Output[,2]))
lines(smooth.spline(Output[,3], Output[,4]))
