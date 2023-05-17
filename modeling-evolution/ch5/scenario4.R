# Hawk-Dove quantitative genetic model

# General assumptions
# 1. The population consists of two types of morphs: Hawk, Dove.
# 2. Morph is determined by multiple genes.
# 3. A hawk interacting with a dove always wins.
# 4. A hawk interacting with a hawk earns a negative payoff.
# 5. A dove interacting with a dove divides the payoff.
# 6. Fitness is equal to some initial quantity plus the payoff.

# Morphs: Hawk = 1, Dove = 2

rm(list=ls())

fitness = function(gm, gf, morph, payoff.mat, npop) {
    opponent <- sample(morph)
    fit <- matrix(0, npop, 1)
    #iterate over payoffs
    for(i.receiver in 1:2) {
        for(i.opponent in 1:2) {
            fit[morph==i.receiver & opponent==i.opponent] <- 3 + payoff.mat[i.receiver, i.opponent]
        }    
    }
    mu.male <- sum(fit*gm) / sum(fit)
    mu.female <- mean(gf)
    mu <- mean(c(mu.male, mu.female))
    return(mu)
}

#### MAIN PROGRAM ####
set.seed(100)
N.Pop = 10^4
Morph = matrix(2, N.Pop, 1)
Payoff.Mat = matrix(c(-1, 0, 8, 4), 2, 2)
# give names to the 4 Payoff cells
A = Payoff.Mat[1,1]
B = Payoff.Mat[1,2]
C = Payoff.Mat[2,1]
D = Payoff.Mat[2,2]

H2 = 0.5       # heritability
Vp = 1         # phenotypic variance
Va = Vp * H2   # additive genetic variance
Ve = Vp - Va   # environmental variance
Mu = 0         # trait mean value
SDa = sqrt(Va)
SDe = sqrt(Ve)

Max.Prop = 20  # number of divisions of proportion
# expected proportion of Hawk
P.Hawks = (B-D) / (B+C-A-D)

# structure to store results
Output = matrix(0, Max.Prop, 2)
# proportion of Hawks
Output[,1] = seq(from=0.01, to=0.95, length=Max.Prop)

for(i in 1:Max.Prop) {
    Prop.H = Output[i, 1]  # initial prop of Hawk
    T.Zero = -qnorm(Prop.H, mean=0, sd=1)
    # generate genetic and environmental values using normal dist
    Mu = 0
    GM = rnorm(N.Pop, mean=Mu, sd=SDa) # genetic value of males
    GF = rnorm(N.Pop, mean=Mu, sd=SDa) # genetic value of females
    EM = rnorm(N.Pop, mean=0, sd=SDe)  # environmental value of males
    PM = GM + EM                       # phenotypic value of males
    # combine phenotypic and genetic values
    Morph[PM > T.Zero] = 1   # Hawks
    Morph[PM <= T.Zero] = 2  # Doves
    N.Hawk = sum(Morph[Morph==1])
    Mu = fitness(GM, GF, Morph, Payoff.Mat, N.Pop)
    # store change in proportion
    Output[i, 2] = 1 - pnorm(q=T.Zero, mean=Mu, sd=1) - Output[i, 1]
}


par(mfrow=c(2,1))

Y.Min = min(Output[,2])
Y.Max = max(Output[,2])
plot(Output[,1], Output[,2], type='l', lty=2, ylim=c(Y.Min, Y.Max), xlab='Initial Prop (Prop.H)', ylab='Change (Prop.H)')
lines(Output[,1], rep(0, Max.Prop)) # theoretical ESS horizontal line
points(P.Hawks, 0, pch='X', cex=2)
# splines for smoothing
lines(smooth.spline(Output[,1], Output[,2]))

# Numerical Solution
print('Finding ESS using a numerical approach')
N.Pop = 100
Morph = matrix(2, N.Pop, 1)
Prop.H = 0.5
T.Zero = -qnorm(Prop.H)

H2 = 0.5       # heritability
Vp = 1         # phenotypic variance
Va = Vp * H2   # additive genetic variance
Ve = Vp - Va   # environmental variance
Mu = 0         # trait mean value
SDa = sqrt(Va)
SDe = sqrt(Ve)

Maxgen = 200
# structure to store results
Output = matrix(0, Maxgen, 2)

for(i in 1:Maxgen) {
    Output[i, 1]  = i
    # generate genetic and environmental values using normal dist
    GM = rnorm(N.Pop, mean=Mu, sd=SDa) # genetic value of males
    GF = rnorm(N.Pop, mean=Mu, sd=SDa) # genetic value of females
    EM = rnorm(N.Pop, mean=0, sd=SDe)  # environmental value of males
    PM = GM + EM                       # phenotypic value of males
    # combine phenotypic and genetic values
    Morph[PM > T.Zero] = 1   # Hawks
    Morph[PM <= T.Zero] = 2  # Doves
    N.Hawk = sum(Morph[Morph==1])
    Output[i, 2] = N.Hawk / N.Pop
    # calculate next iteration
    Mu = fitness(GM, GF, Morph, Payoff.Mat, N.Pop)
}

Startgen = 100
Y.Min = min(Output[,2])
Y.Max = max(Output[,2])
plot(Output[,1], Output[,2], type='l', ylim=c(Y.Min, Y.Max), xlab='Generation', ylab='Proportion of Hawks')
lines(Output[,1], rep(P.Hawks, Maxgen))  # theoretical expectation

Equilibrium = Output[Startgen:Maxgen, 2]
print('Mean Hawks, SD Hawks, Expected Hawks')
print(c(mean(Equilibrium), sd(Equilibrium), P.Hawks))
print(t.test(Equilibrium, mu=P.Hawks))

