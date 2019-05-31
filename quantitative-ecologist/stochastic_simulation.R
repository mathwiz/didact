## Simulate 1000 population trajectories and report the probability that the final
## population is smaller than a certain fraction of its original size

critfrac <- 0.25
trials <- 1000
n <- 50 # number of years
b <- 0.5 # recruitment rate
d <- 0.5 # mortality probability
p0 <- 200 # original size
fin <- rep(0, trials) # vector for final populations

## simulation runs
for (tr in 1:trials) {
    p <- rep(p0, n)
    ## time loop for each run
    for (t in 1:(n - 1)) {
        B <- rpois(1, p[t]*b) # new recruits
        D <- rbinom(1, p[t], d) # deaths
        p[t+1] <- p[t] + B - D
    }
    fin[tr] <- p[n]
}


print(paste("Probability that population is smaller than this fraction of original:", critfrac))
print(length(fin[fin < p0*critfrac]) / trials)

