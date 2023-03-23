rm(list=ls())

pop = function(maxlambda, npop, n.patches, mu, psurv, pmig) {
    h2 <- 0.5; cost <- 0.6
    # randomize growth rate
    lambda <- runif(n.patches, min=0, max=maxlambda)
    p <- pnorm(0, mean=-mu, sd=1)
    q <- 1 - p
    y.nonmigrants <- mu + dnorm(0, mean=mu, sd=1) * h2 / (2*p)
    y.migrants <- mu - dnorm(0, mean=mu, sd=1) * h2 / (2*q)
    emigrants <- pmig * npop * q
    n.migrants <- psurv * sum(emigrants) # term N(t,T)
    y.star <- pmig * sum(npop * psurv * y.migrants * q)
    mu <- npop * (y.nonmigrants * p + y.migrants * q * (1-pmig) * cost) + (y.star/n.patches)
    mu <- mu / (npop * (p + q * (1-pmig) * cost) + (n.migrants / n.patches))
    npop <- npop - emigrants + (n.migrants / n.patches)
    npop <- npop * lambda
    # set extinct populations to exactly zero
    npop[npop < 1] <- 0
    # apply carrying capacity
    npop[npop > Carrying.Capacity] <- Carrying.Capacity
    
    return(c(npop, mu))
}

################ Main Program ####################
set.seed(100)

Max.Gen = 2000
Carrying.Capacity = 1000
N.Init = 20
Max.Lambda = 2.2
N.Patches = 10
P.Mig = 0.8
P.Surv = 0.95
Mu = matrix(0, N.Patches, 1)
# allocate data structures
Npop = matrix(N.Init, N.Patches, 1)
Npop.Sizes = matrix(0, Max.Gen)
N.Extinct = matrix(0, N.Patches, 1)
Mean.Nonmig = matrix(0, Max.Gen)
Mean.Mig = matrix(0, Max.Gen)
# set first generation
Npop.Sizes[1] = mean(Npop)
Mean.Nonmig[1] = mean(pnorm(0, mean=-Mu, sd=1))
Mean.Mig[1] = 1 - Mean.Nonmig[1]

Gen = 1
while (Gen < Max.Gen && Npop.Sizes[Gen] > 0) {
    Gen = Gen + 1
    Out = pop(Max.Lambda, Npop, N.Patches, Mu, P.Surv, P.Mig)
    Npop = Out[1:N.Patches]
    # mean liabilities
    n1 = N.Patches + 1
    n2 = 2 * N.Patches
    Mu = Out[n1:n2]
    P.Nonmigrants = pnorm(0, mean=-Mu, sd=1)
    # mean proportion of nonmigrants in metapopulation
    Mean.Nonmig[Gen] = sum(Npop * P.Nonmigrants) / sum(Npop)
    # mean proportion of populations migrating
    Mean.Mig[Gen] = sum(Npop * (1 - P.Nonmigrants) * P.Mig) / sum(Npop)
    
    Npop.Sizes[Gen] <- mean(Npop)
    N.Extinct[Gen] <- length(Npop[Npop==0])
}    

print("migration finished.")

# Output
output = function() {
    par(mfcol=c(2,2))
    Gens <- seq(1, Gen)
    plot(Gens, Npop.Sizes[1:Gen], xlab="Generation", ylab="Mean Pop Size", type="l")
    plot(Gens, N.Extinct[1:Gen], xlab="Generation", ylab="Number of Pops Extinct", type="l", ylim=c(0, N.Patches))
    plot(Gens, Mean.Nonmig[1:Gen], xlab="Generation", ylab="Mean Proportion Non-migrants", type="l")
    plot(Gens, Mean.Mig[1:Gen], xlab="Generation", ylab="Mean Proportion Migrants", type="l")
}
