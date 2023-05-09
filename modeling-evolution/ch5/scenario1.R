# A frequency-independent game

# General assumptions
# 1. Two trees grow sufficiently close to each other that they interfere with each other's growth.
# 2. Growth is positively related to the amount of leaf tissue and the amount of this that can photosynthesize.
# 3. The amount of leaf tissue is a function of the allocation of growth in height versus the growth in leaf tissue.
# 4. The proportion of leaf tissue declines as biomass is allocated to increasing plant height.
# 5. The amount of photosyntesis is a function of the differenc in height of the trees. The larger tree being potentially capable of having greater photosynthesis per leaf.
# 6. Fitness is a function of the payoff matrix.


rm(list=ls())

payoff = function(xs) {
    alpha <- 3; p.leaf <- 0.25; p.height <- 1.0
    fit <- (1 - xs[2]^alpha)
    g <- p.leaf + (p.height - p.leaf) / (1 + exp(-5*(xs[2]-xs[1])))
    return(fit * g)
}

#### MAIN PROGRAM ####

N = 200    # number of divisions
Height = seq(0, 1, length=N)
D = expand.grid(Height, Height)
Paytemp = apply(D, 1, payoff)
Payoff = matrix(Paytemp, N, N, byrow=T)
Best = matrix(0, N, 1)

# iterate over each height
for(i in 1:N) {
    Index = order(Payoff[,1], na.last=T, decreasing=T)
    Best[i] = Height[Index[i]]
}

par(mfrow=c(1,1))
plot(Height, Best, type='l', xlab='Height of A', ylab='Height of B', xlim=c(0,1), ylim=c(0,1))
lines(Best, Height, lty=2)
