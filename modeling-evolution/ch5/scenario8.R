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

#### MAIN PROGRAM ####
set.seed(100)

P = matrix(seq(from=0.01, to=0.99, length=20), 20, 1)  # proportion of satellites
FF = apply(P, 1, delta.rho)

plot(P, FF, type='l', xlab='Proportion territorial males, rho', ylab='Delta rho')
lines(c(0,1), c(0,0), lty=2)

print(c('Analytical root'))
print(uniroot(delta.rho, interval=c(0.01, 0.99))$root)
