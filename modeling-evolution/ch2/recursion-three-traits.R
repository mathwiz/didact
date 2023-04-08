rm(list=ls())

Mj = 0.05
Ma = 0.40
Fecundity = 0.05
A = 10 # Wt increase/annum without reproduction

fitness = function(g, alpha, n) {
    age.max <- alpha + 1
    alpha.minus.1 <- alpha - 1
    surv <- matrix(0, age.max)
    # growth prior to reproduction is linear
    wt <- matrix(0, age.max, 1)
    surv[1] <- exp(-Mj)
    
    for(i in 2:alpha.minus.1) {
        wt[i] <- wt[i-1] + A
        surv[i] <- surv[i-1]*exp(-Mj)
    }
    # now for age alpha to max
    # now accumulate w inside loop
    w <- 0
    for(j in 1:n) {
        i <- alpha + j - 1
        wt[i] <- wt[i-1] + A - g[j]*wt[i-1]
        surv[i] <- surv[i-1]*exp(-(Mj + Ma*g[j]))
        w <- w + Fecundity*surv[i]*wt[i]*g[j]
    }

    return(-w)
}    

best.g = function(alpha) {
    n <- 2 # number of traits
    g1 <- nlm(fitness, p=rep(0.1, times=n), alpha, n)$estimate
    w1 <- fitness(g1, alpha, n)
    g2 <- nlm(fitness, p=rep(0.1, times=n), alpha + 1, n)$estimate
    w2 <- fitness(g2, alpha + 1, n)
    wdiff <- w1 - w2
    return(c(wdiff, w1, g1))
}


#### Main program ####

# brute force approach
Alpha = 5
Diff = best.g(Alpha)
while(Diff[1] > 0) { # if Diff[1] > 0 then W is still increasing
    Alpha = Alpha + 1
    Diff = best.g(Alpha) #Diff[3] = G, Diff[2] = -W
}


print('recursion-three-traits done.')

output = function() {
    par(mfrow=c(1,1))
    print('Brute force approach')
    print(c(Alpha, Diff))
}    
