# Frequency-dependence with limited interactions

# General Assumptions
# 1. A population consists of territorial and satellite males, the latter attempting to sneak copulations rather than defending a territory. A population can consist of only territorial males but not solely satellite males.
# 2. Territorial males do not interact with each other but satellite males interact with territorial males and other satellite males.
# 3. Territorial males are more successful at obtaining mates but suffer a reduction in fitness as a consequence of some other factor. For example, territorial males of certain cricket species are subject to parasitism by an acoustically orienting fly (Cade 1975, 1984) and territorial males of some fish species must delay maturity to achieve a size at which they are able to defend a territory.
# 4. The fitness of a satellite male is reduced by the presence of other satellite males attached to the "focal" territorial male.
# 5. The fitness of a territorial male is reduced by the presence of satellite males.


rm(list=ls())

delta.rho = function(prop.t) {
    n <- 1000
    n.t <- prop.t * n
    n.s <- n - n.t
    n.s.per.t <- n.s / n.t
    s.fit <- 5 / n.s.per.t
    s.fit.total = n.t * s.fit
    t.fit.total = 0.8 * n.t
    return(prop.t - t.fit.total / (t.fit.total + s.fit.total))
}    

# Add variation in satellite males
# 1. Generate a vector of the sequence 1 through n.s as the vector of indices.
# 2. Use apply function to generate binomial probs.
# 3. Calculate the vector of fitness values for each grouping of satellite males.
# 4. Calculate the "corrected" probability of each grouping and multiply by the number of territorial males with satellites.
# 5. Sum the above to obtain the total fitness of satellite males.

## binom = function(x, nn, prob.y) { dbinom(x, size=nn, prob.y) }

delta.rho2 = function(prop.t) {
    n <- 1000
    n.t <- round(prop.t * n)
    n.s <- n - n.t
    binom.prob <- 1 / n.t
    p.zero <- dbinom(x=0, size=n.s, binom.prob)  # prob of no satellites
    n.t.zero <- n.t * p.zero  # number of territorials without satellites
    # number of satellites per territorial
    n.s.per.t <- matrix(seq(from=1, to=n.s), n.s, 1)
    # probability of 1, 2, 3,..., n.s satellites
    prob.x <- apply(n.s.per.t, 1, dbinom, n.s, binom.prob)
    # fitness of satellite for each number per territorial
    s.fit <- 5 / n.s.per.t
    # frequency distribution of satellites per territorial
    freq <- (prob.x * (n.t - n.t.zero)) / (1 - p.zero)
    
    s.fit.total = sum(freq * s.fit)
    t.fit.total = n.t * p.zero + 0.8 * n.t * (1 - p.zero)
    return(prop.t - t.fit.total / (t.fit.total + s.fit.total))
}    

#### MAIN PROGRAM ####
set.seed(100)
par(mfrow=c(2, 2))

P = matrix(seq(from=0.01, to=0.99, length=20), 20, 1)  # proportion of satellites
FF = apply(P, 1, delta.rho)

plot(P, FF, type='l', xlab='Proportion territorial males, rho', ylab='Delta rho')
lines(c(0,1), c(0,0), lty=2)

print(c('Analytical root 1'))
print(uniroot(delta.rho, interval=c(0.01, 0.99))$root)

FF2 = apply(P, 1, delta.rho2)

plot(P, FF2, type='l', xlab='Proportion territorial males, rho', ylab='Delta rho')
lines(c(0,1), c(0,0), lty=2)

print(c('Analytical root 2'))
print(uniroot(delta.rho2, interval=c(0.01, 0.99))$root)


# Find ESS with numerical approach

fitness = function(p.t, n.pop) {
    max.sat <- 5  # maximum number of satellites per territorial
    n.t <- round(n.pop * p.t)
    n.s <- n.pop - n.t
    # index labels territorial males
    x <- seq(from=1, to=n.t)
    # draw n.s integers from x. This is the territorial assigned to the satellite.
    matches <- sample(x, n.s, replace=T)
    # find number of satellites for each territorial
    s.per.t.table <- table(matches)
    # find number of satellite matings
    n.t.s <- length(s.per.t.table)
    table.vec <- matrix(s.per.t.table, , 1)  # turn into column vector
    # total fitness of satellites and territorials
    w.s <- sum(max.sat / table.vec)
    w.t <- (n.t - n.t.s)*1.0 + n.t.s*0.8
    # total fitness
    w <- w.t + w.s
    freq.t <- w.t / w
    
    return(freq.t)
}    

# Reset parameters
N.Pop = 1000
P.T = 0.25  # initial proportion of territorials
Maxgen = 100
Startgen = 40
Output = matrix(0, Maxgen, 2)
Output[, 1] = seq(from=1, to=Maxgen)
Output[1, 2] = P.T

for(i in 2:Maxgen) {
    Output[i, 2] = fitness(Output[i-1, 2], N.Pop)
}

plot(Output[, 1], Output[, 2], type='l', xlab='Generation', ylab='Proportion Territorials')

print(c('Mean frequency averaged from generation', Startgen))
print(mean(Output[Startgen:Maxgen, 2]))
