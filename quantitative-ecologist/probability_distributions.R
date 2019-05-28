## Egg cluster data
m<- c(20:40)
## values of the discrete PMF for egg clusters
pm<- c(rep(0,6), (1/30)*((26:30) - 25), (1/30)*(36 - (31:35)), rep(0,5))

## Discrete CDF
cm<- cumsum(pm)
cm(0)
cm(10)
cm(32)
cm(36)
cm(37)
cm(40)


                                        # Examples of PDF, CDF, Quantiles, Random generators

                                        # Normal distribution

?dnorm
dnorm(-2, 0, 1) # x, mu, sigma
pnorm(0, 0, 1) # x, mu, sigma
qnorm(.05, 0, 1) # quantile, mu, sigma
rnorm(10, 0, 1) # n, mu, sigma


                                        # Uniform distribution

?dunif
dunif(0, -1, 1) # x, min, max
dunif(0.5, -1, 1) # x, min, max

punif(0, -1, 1) # x, min, max
punif(0.5, -1, 1) # x, min, max

qunif(0.25, -1, 1) # quantile, min, max
qunif(0.5, -1, 1) # quantile, min, max
qunif(0.75, -1, 1) # quantile, min, max

runif(10, -1, 1) # n, min, max
## equivalent formulations for discrete
round(runif(10, -1-.5, 1+.5))
sample(-1:1, replace=T, 10)


                                        # Binomial distribution

choose(8, 4)
factorial(8) / (factorial(4) * factorial(4))

?dbinom
dbinom(x=4, size=8, p=.8)
pbinom(q=c(.25, .5, .75), size=8, p=.8)
qbinom(p=.3, prob=.8, size=8, lower.tail=TRUE)
rbinom(n=4, size=8, prob=.8)


                                        # Multinomial distribution

?dmultinom
p<- c(0.4, 0.2, 0.1, 0.1, 0.15, 0.05)
x<- c(.1, .1, .2, .2, .1, .1)
sum(x)
dmultinom(x=6, size=6, prob=.25) # not sure what this does
rmultinom(1, 120, p)


                                        # Poisson distribution

?dpois
dpois(x=16, lambda=12) 
ppois(q=16, lambda=12) # lower tail
qpois(p=.25, lambda=12) # lower tail
rpois(n=10, lambda=12) 


                                        # Discrete waiting distributions

                                        # Geometric distribution
                                        # Interp.: Number of trials until next success

?dgeom
failures = 4 # number of failures in Bernoulli trials before success
psuccess = .25 # probability of success
prob= .75 # probability of success
dgeom(x=failures, prob=psuccess)
pgeom(q=failures, prob=psuccess, lower.tail=TRUE)
qgeom(p=prob, prob=psuccess, lower.tail=TRUE)
rgeom(n=10, prob=psuccess) # the interpretation described above


                                        # Negative binomal distribution
                                        # Interp.: Number of trials until k successes

?dnbinom
quantile <- 10
k <- 5
prob <- .5
dnbinom(x=quantile, size=k, prob=prob)
pnbinom(q=quantile, size=k, prob=prob)
dnbinom(x=quantile, size=k, prob=prob)
qnbinom(p=.25, size=100, prob=prob)
rnbinom(n=10, size=5, prob=.5) # the interpretation described above


                                        # Continuous waiting distributions

                                        # Exponential distribution
                                        # Interp.: Waiting time (or distance) until next occurrence

?dexp
k <- 1
lambda <- 5
dexp(x=1, rate = lambda, log = FALSE)
pexp(q=1, rate = lambda, lower.tail = TRUE, log.p = FALSE)
qexp(p=.5, rate = lambda, lower.tail = TRUE, log.p = FALSE)
rexp(n=10, rate = lambda)


                                        # Gamma distribution
                                        # Interp.: Waiting time (or distance) until kth occurrence

?dgamma
k <- 5
lambda <- 5
dgamma(x=k, shape=lambda, log = FALSE)
pgamma(q=k, shape=lambda, lower.tail = TRUE, log.p = FALSE)
qgamma(p=.25, shape=lambda, lower.tail = TRUE, log.p = FALSE)
rgamma(n=10, shape=lambda)








