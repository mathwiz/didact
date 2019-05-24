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
                                        # for Normal distribution

dnorm(-2, 0, 1) # x, mu, sigma
pnorm(0, 0, 1) # x, mu, sigma
qnorm(.05, 0, 1) # quantile, mu, sigma
rnorm(10, 0, 1) # n, mu, sigma

                                        # Uniform distribution

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

