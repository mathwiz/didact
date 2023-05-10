# Hawk-Dove clonal model

# General assumptions
# 1. The population consists of two types of clones: Hawk, Dove.
# 2. A hawk interacting with a dove always wins.
# 3. A hawk interacting with a hawk earns a negative payoff.
# 4. A dove interacting with a dove divides the payoff.
# 5. Fitness is equal to some initial quantity plus the payoff.

# Code: Hawk=1, Dove=2

rm(list=ls())

fitness = function(morph, npop, payoff.mat) {
    opponent <- sample(morph)
    fit <- matrix(0, npop, 1)
    for(i.receiver in 1:2) {
        for(i.opponent in 1:2) {
            fit[morph==i.receiver & opponent ==i.opponent] <- 3 + payoff.mat[i.receiver, i.opponent]
        }    
    }
    p.hawk <- sum(fit[morph==1]) / sum(fit) # mean fitness of hawks
    return(p.hawk)
}

#### MAIN PROGRAM ####
set.seed(100)
N.Pop = 1000
Maxgen = 100
Output = matrix(0, Maxgen, 2)
P.Hawk = 0.5
N.Hawk = N.Pop * P.Hawk
Morph = matrix(2, N.Pop, 1)
Morph[1:N.Hawk] = 1  # make the correct proportion Hawks
Payoff.Mat = matrix(c(-1, 0, 8, 4), 2, 2)
# give names to the 4 Payoff cells
A = Payoff.Mat[1,1]
B = Payoff.Mat[1,2]
C = Payoff.Mat[2,1]
D = Payoff.Mat[2,2]
P.Hawks = (B-D) / (B+C-A-D)

for(i in 1:Maxgen) {
    Output[i, 1] = i
    N.Hawk = length(Morph[Morph==1])
    Output[i, 2] = N.Hawk / N.Pop
    P.Hawk = fitness(Morph, N.Pop, Payoff.Mat)
    N.Hawk = round(N.Pop * P.Hawk)
    Morph[1:N.Pop, 1] = 2 # initialize to Dove
    Morph[1:N.Hawk, 1] = 1 # make correct number of Hawks
}


par(mfrow=c(1,1))
plot(Output[,1], Output[,2], type='l', xlab='Generation', ylab='Proportion Hawk')
lines(Output[,1], rep(P.Hawk, Maxgen), lty=2) # theoretical number of Hawks

print('t-test vs theoretical expectation (after generation 20) [mean, sd, theoretical]')
Equilibrium.P.Hawk = Output[20:Maxgen, 2]
print(c(mean(Equilibrium.P.Hawk), sd(Equilibrium.P.Hawk), P.Hawks))
print(t.test(Equilibrium.P.Hawk, mu=P.Hawks))
